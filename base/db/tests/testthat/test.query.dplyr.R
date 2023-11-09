test_that("`fancy_scientific()` converts numbers to scientific expressions with proper formatting", {
  result <- fancy_scientific(1234567890)
  expect_equal(result, expression("1.234568" %*% 10^+9))

  result <- fancy_scientific(0.00000123)
  expect_equal(result, expression("1.23" %*% 10^-6))

  result <- fancy_scientific(1e-20)
  expect_equal(result, expression("1" %*% 10^-20))
})

test_that("`dplyr.count()` returns the correct count of rows in a dataframe", {
  
  df <- data.frame(
    x = c(1, 2, 3, 2, 1, 3),
    y = c("a", "b", "a", "b", "a", "b")
  )
  result <- dplyr.count(df)
  expect_equal(result, 6)

  df <- data.frame()
  result <- dplyr.count(df)
  expect_equal(result, 0)
})

test_that("`dbHostInfo()` able to return correct host information", {
  mockery::stub(dbHostInfo, 'db.query', data.frame(floor = 10))
  mockery::stub(
    dbHostInfo, 
    'dplyr::tbl', 
    data.frame(
      data.frame(
        sync_host_id = c(10, 11),
        hostname = c("test_host_1", "test_host_2"),
        sync_start = c("20190201", "20190201"),
        sync_end = c("20200101", "20200101"),
        sync_url = c("http://test_url_1", "http://test_url_2"),
        sync_contact = c("test_contact_1", "test_contact_2")
      )
    )
  ) 
  result <- dbHostInfo(bety = 1)
  expect_equal(result$hostid, 10)
  expect_equal(result$hostname, "test_host_1")
  expect_equal(result$start, "20190201")
  expect_equal(result$end, "20200101")
  expect_equal(result$sync_url, "http://test_url_1")
  expect_equal(result$sync_contact, "test_contact_1")
})

test_that("`workflows()` able to correctly return a list of workflows", {
  mockery::stub(
    workflows, 
    'dbHostInfo', 
    list(
      hostid = 10,
      hostname = "test_host_1",
      start = 3,
      end = 10,
      sync_url = "http://test_url_1",
      sync_contact = "test_contact_1"
    )
  )
  mockery::stub(workflows, 'dplyr::tbl', data.frame(workflow_id = c(1, 2, 3, 4, 5, 6)))
  result <- workflows(bety = 1, ensemble = TRUE)
  expect_equal(result, data.frame(workflow_id = c(3, 4, 5, 6)))
})

test_that("`workflow()` able to get a workflow data by id", {
  mockery::stub(
    workflow, 
    'workflows', 
    data.frame(workflow_id = c(1, 2, 3, 4, 5, 6), workflow_name = c("A", "B", "C", "D", "E", "F"))
  )
  result <- workflow(bety = 1, workflow_id = 3)
  expect_equal(result, data.frame(workflow_id = 3, workflow_name = "C"))
})

test_that("`runs()` is able to get table of runs for a corresponding workflow", {
  mockery::stub(
    runs, 
    'workflow', 
    data.frame(
      workflow_id = c(1, 1),
      folder = c("test_folder_1", "test_folder_2")
    )
  )
  mocked_res <- mockery::mock(
    data.frame(
      id = c(1, 2, 3, 4, 5, 6),
      workflow_id = c(1, 1, 3, 4, 5, 6)
    ),
    data.frame(
      id = c(1, 2, 3),
      ensemble_id = c(1, 1, 2)
    )
  )
  mockery::stub(runs, 'dplyr::tbl', mocked_res)
  result <- runs(bety = 1, workflow_id = 1)
  expect_equal(result$run_id, c(1, 1, 2, 2, 3, 3))
  expect_equal(result$folder, c("test_folder_1", "test_folder_2", "test_folder_1", "test_folder_2", "test_folder_1", "test_folder_2"))
})

test_that("`get_workflow_ids()` able to get a vector of unique workflow IDs", {
  mockery::stub(
    get_workflow_ids,
    'workflows',
    data.frame(
      workflow_id = c(1, 2, 2, 3, 4, 4),
      workflow_name = c("A", "B", "C", "D", "E", "F")
    )
  )
  result <- get_workflow_ids(bety = 1, query = 1, all.ids = TRUE)
  expect_equal(result, c(4, 3, 2, 1))
})

test_that("`get_users()` ", {
  mockery::stub(get_users, 'dplyr::tbl', data.frame(id = c(20200101, 20200102, 20240103)))
  mockery::stub(
    get_users, 
    'dbHostInfo',
    data.frame(
      start = 20190201,
      end = 20230101
    )
  )
  result <- get_users(bety = 1)
  expect_equal(result, data.frame(id = c(20200101, 20200102)))
})

test_that("`get_run_ids()` able to get vector of run ids (in sorted order) for a given workflow ID", {
  mockery::stub(
    get_run_ids,
    'runs',
    data.frame(
      run_id = c(3, 1, 2),
      folder = c("test_folder_1", "test_folder_2", "test_folder_3")
    )
  )

  result <- get_run_ids(bety = 1, workflow_id = 1)
  expect_equal(result, c(1, 2, 3))

  # if no run ids are found
  mockery::stub(get_run_ids, 'runs', data.frame())
  result <- get_run_ids(bety = 1, workflow_id = 1)
  expect_equal(result, c("No runs found"))
})

test_that("`var_names_all()` able get vector of variable names for a particular workflow and run ID removing variables not to be shown to user", {
  mockery::stub(var_names_all, 'get_var_names', c('A', 'B', 'C', 'Year','FracJulianDay'))
  result <- var_names_all(bety = 1, workflow_id = 1, run_id = 1)
  expect_equal(result, c('A', 'B', 'C'))
})