{application, s3images, [
    {description, "A webservice for storing images on Amazon S3."},
    {vsn, "0.1"},
    {modules, [
        s3images,
        s3images_ahandler,
        s3images_ihandler,
        s3images_image,
        s3images_util,
        s3images_sup,
        s3images_yaws
    ]},
    {registered, [s3images]},
    {applications, [kernel, stdlib, sasl, crypto, yaws, mnesia, inets]},
    {mod, {s3images, []}},
    {env, [
      {ip, {0, 0, 0, 0}},
      {port, 5056},
      {domain, "localhost"},
      {s3bucket, "media.example.com"},
      {s3key, "keyabc123"},
      {s3secret, "secretabc123"}
    ]},
    {start_phases, [
      {mnesia, []},
      {s3, []}
    ]}
]}.