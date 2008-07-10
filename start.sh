#!/bin/bash
erl -setcookie s3imagespass -sname s3imagesapp -pa ebin -yaws embedded true -boot start_sasl -mnesia dir 's3images.mnesia' 
