CONFIG = docker.json

json: clean
	dhall-to-json --file docker.dhall --output $(CONFIG)

clean:
	rm -f $(CONFIG)
