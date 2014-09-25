-record(todo, {
          id :: string(),
          title :: string(),
          dateDue :: calendar:date() | undefined,
          notes :: string() | undefined,
          dateCreated :: calendar:date(),
          dateUpdated :: calendar:date(),
          complete = false :: boolean()
         }).
