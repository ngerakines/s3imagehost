
-define(DIR, "/tmp/").

-record(image, {name, bucket, object, creator, create_date = 0, userdata = []}).
