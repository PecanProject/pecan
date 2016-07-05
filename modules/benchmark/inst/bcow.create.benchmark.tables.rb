class CreateBenchmarksTables < ActiveRecord::Migration
  def self.up
    # Create the table as normal, then change id to bigint, so all
    # pieces are created, like index and auto increment. This is
    # done for all tables with an :id

    create_table :benchmarks do |t|
      t.integer :input_id, :limit => 8, :null => false
      t.text    :description
      t.integer :site_id, :limit => 8, :null => false
      t.integer :variable_id, :limit => 8, :null => false
      t.integer :user_id, :limit => 8
      t.datetime :created_at
      t.datetime :updated_at
    end
    change_column :benchmarks, :id, :integer, :limit => 8

    execute %{
      ALTER TABLE "benchmarks"
      ALTER COLUMN created_at SET DEFAULT utc_now(),
      ALTER COLUMN updated_at SET DEFAULT utc_now();

      ALTER TABLE "benchmarks"
      ADD CONSTRAINT "benchmarks_input_id_fkey" 
        FOREIGN KEY ("input_id") REFERENCES inputs("id")
        ON DELETE CASCADE ON UPDATE CASCADE,
      ADD CONSTRAINT "benchmarks_site_id_fkey" 
        FOREIGN KEY ("site_id") REFERENCES sites("id")
        ON DELETE CASCADE ON UPDATE CASCADE,
      ADD CONSTRAINT "benchmarks_variable_id_fkey" 
        FOREIGN KEY ("variable_id") REFERENCES variables("id")
        ON DELETE CASCADE ON UPDATE CASCADE;
    }

    create_table :metrics do |t|
      t.string :name
      t.text :description 
      t.integer :citation_id, :limit => 8
      t.integer :user_id, :limit => 8
      t.datetime :created_at
      t.datetime :updated_at
    end
    change_column :metrics, :id, :integer, :limit => 8
    execute %{
      ALTER TABLE "metrics"
      ALTER COLUMN created_at SET DEFAULT utc_now(),
      ALTER COLUMN updated_at SET DEFAULT utc_now();
    }

    create_table :benchmarks_ensembles do |t|
      t.integer :reference_run_id, :limit => 8, :null => false
      t.integer :ensemble_id, :limit => 8, :null => false
      t.integer :model_id, :limit => 8, :null => false
      t.integer :citation_id, :limit => 8, :null => false
      t.integer :user_id, :limit => 8
      t.datetime :created_at
      t.datetime :updated_at
    end
    change_column :benchmarks_ensembles, :id, :integer, :limit => 8

    execute %{
      ALTER TABLE "benchmarks_ensembles"
      ALTER COLUMN created_at SET DEFAULT utc_now(),
      ALTER COLUMN updated_at SET DEFAULT utc_now();

      ALTER TABLE "benchmarks_ensembles"
      ADD CONSTRAINT "benchmarks_ensembles_ensemble_id_fkey" 
        FOREIGN KEY ("ensemble_id") REFERENCES ensembles("id")
        ON DELETE CASCADE ON UPDATE CASCADE,
      ADD CONSTRAINT "benchmarks_ensembles_model_id_fkey" 
        FOREIGN KEY ("model_id") REFERENCES models("id")
        ON DELETE CASCADE ON UPDATE CASCADE,
      ADD CONSTRAINT "benchmarks_ensembles_reference_run_id_fkey" 
        FOREIGN KEY ("reference_run_id") REFERENCES reference_runs("id") 
        ON DELETE CASCADE ON UPDATE CASCADE;
    }

    create_table :benchmarks_ensembles_scores, :id => false do |t|
      t.integer :benchmarks_ensemble_id, :limit => 8, :null => false
      t.integer :benchmark_id, :limit => 8, :null => false
      t.integer :metric_id, :limit => 8, :null => false
      t.integer :user_id, :limit => 8
      t.datetime :created_at
      t.datetime :updated_at
    end

    execute %{
      ALTER TABLE "benchmarks_ensembles_scores"
      ALTER COLUMN created_at SET DEFAULT utc_now(),
      ALTER COLUMN updated_at SET DEFAULT utc_now();

      ALTER TABLE "benchmarks_ensembles_scores"
      ADD CONSTRAINT "benchmarks_ensembles_scores_benchmark_id_fkey" 
        FOREIGN KEY ("benchmark_id") REFERENCES benchmarks("id")
        ON DELETE CASCADE ON UPDATE CASCADE,
      ADD CONSTRAINT "benchmarks_ensembles_scores_benchmarks_ensemble_id_fkey" 
        FOREIGN KEY ("benchmarks_ensemble_id") REFERENCES benchmarks_ensembles("id")
        ON DELETE CASCADE ON UPDATE CASCADE,
      ADD CONSTRAINT "benchmarks_ensembles_scores_metric_id_fkey" 
        FOREIGN KEY ("metric_id") REFERENCES metrics("id")
        ON DELETE CASCADE ON UPDATE CASCADE;
    }

    create_table :reference_runs do |t|
      t.integer :model_id, :limit => 8 
      t.text :settings 
      t.integer :user_id, :limit => 8
      t.datetime :created_at
      t.datetime :updated_at
    end
    change_column :reference_runs, :id, :integer, :limit => 8

    execute %{
      ALTER TABLE "reference_runs"
      ALTER COLUMN created_at SET DEFAULT utc_now(),
      ALTER COLUMN updated_at SET DEFAULT utc_now();

      ALTER TABLE "reference_runs"
      ADD CONSTRAINT "reference_runs_model_id_fkey" 
        FOREIGN KEY ("model_id") REFERENCES models("id")
        ON DELETE CASCADE ON UPDATE CASCADE;
    }

    create_table :benchmark_sets do |t|
      t.string :name, :null => false
      t.text :description
      t.integer :user_id, :limit => 8
      t.datetime :created_at
      t.datetime :updated_at
    end
    change_column :benchmark_sets, :id, :integer, :limit => 8

    execute %{
      ALTER TABLE "benchmark_sets"
      ALTER COLUMN created_at SET DEFAULT utc_now(),
      ALTER COLUMN updated_at SET DEFAULT utc_now();
    }

    create_table :benchmarks_metrics, :id => false do |t|
      t.integer :benchmark_id, :limit => 8
      t.integer :metric_id, :limit => 8
    end

    execute %{
      ALTER TABLE "benchmarks_metrics"
      ALTER COLUMN created_at SET DEFAULT utc_now(),
      ALTER COLUMN updated_at SET DEFAULT utc_now();

      ALTER TABLE "benchmarks_metrics"
      ALTER COLUMN "benchmarks_metrics_benchmark_id_fkey" 
        FOREIGN KEY ("benchmark_id") REFERENCES benchmarks("id")
        ON DELETE CASCADE ON UPDATE CASCADE,
      ALTER COLUMN "benchmarks_metrics_metric_id_fkey" 
        FOREIGN KEY ("metric_id") REFERENCES metrics("id")
        ON DELETE CASCADE ON UPDATE CASCADE;
    }

    create_table :benchmarks_benchmarks_reference_runs, :id => false do |t|
      t.integer :benchmark_id, :limit => 8
      t.integer :reference_run_id, :limit => 8
    end

    execute %{
      ALTER TABLE "benchmarks_benchmarks_reference_runs"
      ALTER COLUMN created_at SET DEFAULT utc_now(),
      ALTER COLUMN updated_at SET DEFAULT utc_now();

      ALTER COLUMN "benchmarks_benchmarks_reference_runs_benchmark_id_fkey" 
        FOREIGN KEY ("benchmark_id") REFERENCES benchmarks("id")
        ON DELETE CASCADE ON UPDATE CASCADE,
      ALTER COLUMN "benchmarks_benchmarks_reference_runs_reference_run_id_fkey" 
        FOREIGN KEY ("reference_run_id") REFERENCES reference_runs("id")
        ON DELETE CASCADE ON UPDATE CASCADE;
    }

    create_table :benchmark_sets_benchmark_reference_runs, :id => false do |t|
      t.integer :benchmark_set_id, :limit => 8
      t.integer :reference_run_id, :limit => 8
    end  
    
    execute %{
      ALTER TABLE "benchmark_sets_benchmark_reference_runs"
      ALTER COLUMN created_at SET DEFAULT utc_now(),
      ALTER COLUMN updated_at SET DEFAULT utc_now();

      ALTER COLUMN "benchmark_sets_benchmark_reference_runs_benchmark_set_id_fkey" 
        FOREIGN KEY ("benchmark_set_id") REFERENCES benchmark_sets("id")
        ON DELETE CASCADE ON UPDATE CASCADE,
      ALTER COLUMN "benchmark_sets_benchmark_reference_runs_reference_run_id_fkey" 
        FOREIGN KEY ("reference_run_id") REFERENCES reference_runs("id")
        ON DELETE CASCADE ON UPDATE CASCADE;
    }
    end

    def self.down
      drop_table :benchmarks
      drop_table :metrics
      drop_table :benchmarks_ensembles
      drop_table :benchmarks_ensembles_scores
      drop_table :reference_runs
      drop_table :benchmark_sets
      drop_table :benchmarks_metrics
      drop_table :benchmarks_benchmarks_reference_runs
      drop_table :benchmark_sets_benchmark_reference_runs
    end
  end

