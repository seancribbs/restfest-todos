-record(todo, {
          id :: string(),
          title :: string(),
          dateDue :: calendar:date() | undefined,
          notes = "" :: string(),
          dateCreated = element(1, calendar:local_time()) :: calendar:date(),
          dateUpdated = element(1, calendar:local_time()) :: calendar:date(),
          complete = false :: boolean()
         }).
