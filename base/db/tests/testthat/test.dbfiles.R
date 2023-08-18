test_that("`dbfile.posterior.insert()` able to make a correct query to insert a file into dbfiles table as a posterior", {
  mocked_res <- mockery::mock(NULL, NULL, data.frame(id = 10))
  mockery::stub(dbfile.posterior.insert, 'get.id', 1)
  mockery::stub(dbfile.posterior.insert, 'dbfile.insert', 1010)
  mockery::stub(dbfile.posterior.insert, 'db.query', mocked_res)

  dbfile.posterior.insert('trait.data.Rdata', 'test-pft', 'application/x-RData', 'traits', con = NULL)
  args <- mockery::mock_args(mocked_res)
  expect_true(grepl("INSERT INTO posteriors \\(pft_id, format_id\\) VALUES \\(1, 1\\)", args[[2]]$query))

})

test_that("`dbfile.posterior.check()` able to form the correct query to retrieve correct posterior id to run further checks", {
  mocked_res <- mockery::mock(data.frame(id = 2020))
  mockery::stub(dbfile.posterior.check, 'get.id', 1)
  mockery::stub(dbfile.posterior.check, 'db.query', mocked_res)
  mockery::stub(dbfile.posterior.check, 'dbfile.check', data.frame(id = 1, filename = 'test_1', pathname = 'path_1'))

  dbfile.posterior.check('testpft', 'application/x-RData', 'traits', con = NULL)

  args <- mockery::mock_args(mocked_res)
  expect_true(
    grepl(
      "SELECT id FROM posteriors WHERE pft_id=1 AND format_id=1", 
      args[[1]]$query
    )
  )
})

test_that("`dbfile.insert()` able to add correct parameter values to the insert database query and return a file id", {
  mocked_res <- mockery::mock(data.frame(), data.frame(id = 2020))
  mockery::stub(dbfile.insert, 'get.id', 1)
  mockery::stub(dbfile.insert, 'db.query', mocked_res)
  
  res <- dbfile.insert(in.path = '/test/file/path', in.prefix = 'testfile.txt', 'Input', 7, con = NULL)
  args <- mockery::mock_args(mocked_res)
  expect_equal(res, 2020)
  expect_true(grepl("VALUES \\('Input', 7, 'testfile.txt', '/test/file/path', 1\\) RETURNING id", args[[2]]$query))
})

test_that("`dbfile.check()` able to return the most recent entries from `dbfiles` table associated with a container and machine", {
  mockery::stub(dbfile.check, 'get.id', 1)
  mockery::stub(
    dbfile.check, 
    'dplyr::tbl', 
    data.frame(
      container_type = c('Input', 'Input', 'Model'), 
      container_id = c(7, 7, 7),
      machine_id = c(1, 1, 2), 
      updated_at = c(20201112, 20210101, 20210102), 
      id = c(2, 3, 4), 
      filename = c('test_1', 'test_2', 'test_3'),
      pathname = c('path_1', 'path_2', 'path_3')
    )
  )
  res <- dbfile.check("Input", 7, con = NULL)
  
  expect_equal(
    res, 
    data.frame(container_type = 'Input', container_id = 7, machine_id = 1, updated_at = 20210101, id = 3, filename = 'test_2', pathname = 'path_2')
  )
})

test_that("`dbfile.file()` able to return a correctly formed file path from entries in the `dbfiles` table for a particular container and machine", {
  mockery::stub(dbfile.file, 'dbfile.check', data.frame(file_path = 'test/dir/path', file_name = 'test_file'))
  expect_equal(dbfile.file('Input', 7, con = NULL), file.path('test/dir/path/test_file'))
})

test_that("`dbfile.id()` able to construct a correct database query to get id for a dbfile given the container type and filepath", {
  mocked_res <- mockery::mock(data.frame(id = 1), data.frame(container_id = 2020))
  mockery::stub(dbfile.id, 'db.query', mocked_res)

  res <- dbfile.id('Model', '/usr/local/bin/sipnet', con = NULL)
  args <- mockery::mock_args(mocked_res)
  
  expect_equal(res, 2020)
  expect_true(
    grepl(
      "WHERE container_type='Model' AND file_path='/usr/local/bin' AND file_name='sipnet' AND machine_id=1", 
      args[[2]]$query
    )
  )
})
