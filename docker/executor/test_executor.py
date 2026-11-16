"""Regression tests for the RabbitMQ workflow executor."""
import importlib.util
import json
import pathlib
import sys
import tempfile
import types
import unittest
from unittest import mock


EXECUTOR = pathlib.Path(__file__).with_name("executor.py")
with mock.patch.dict(sys.modules, {"pika": types.ModuleType("pika")}):
    spec = importlib.util.spec_from_file_location("pecan_executor", EXECUTOR)
    executor = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(executor)


class WorkerCompletionTests(unittest.TestCase):
    def make_worker(self, body):
        return executor.Worker(types.SimpleNamespace(delivery_tag=1), None, body)

    def test_malformed_json_releases_worker(self):
        worker = self.make_worker(b"{broken")
        with self.assertLogs(level="ERROR") as logs:
            worker.runfunc()
        self.assertTrue(worker.finished)
        self.assertIn("JSONDecodeError", "\n".join(logs.output))

    def test_non_object_json_releases_worker(self):
        worker = self.make_worker(b"[]")
        with self.assertLogs(level="ERROR") as logs:
            worker.runfunc()
        self.assertTrue(worker.finished)
        self.assertIn("AttributeError", "\n".join(logs.output))

    def test_setup_error_writes_error_status(self):
        with tempfile.TemporaryDirectory() as folder:
            job_folder = pathlib.Path(folder) / "job"
            worker = self.make_worker(json.dumps({
                "folder": str(job_folder), "pecan_xml": "<pecan/>"
            }).encode())
            with mock.patch.object(executor.shutil, "copyfile", side_effect=OSError("missing workflow")):
                with self.assertLogs(level="ERROR") as logs:
                    worker.runfunc()
            self.assertTrue(worker.finished)
            self.assertIn("missing workflow", "\n".join(logs.output))
            self.assertEqual(
                (job_folder / "rabbitmq.out").read_text(),
                "OSError: missing workflow\nERROR\n",
            )

    def test_valid_job_still_completes_and_writes_status(self):
        with tempfile.TemporaryDirectory() as folder:
            worker = self.make_worker(json.dumps({"folder": folder}).encode())
            with mock.patch.object(executor.subprocess, "check_output", return_value=b"done"):
                worker.runfunc()
            self.assertTrue(worker.finished)
            self.assertIn("OK", (pathlib.Path(folder) / "rabbitmq.out").read_text())


if __name__ == "__main__":
    unittest.main()
