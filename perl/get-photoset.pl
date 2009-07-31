#!/usr/bin/env perl

use Flickr::API;

my $api = new Flickr::API({'key'    => 'f9cbdebffb9c269192841aae0ba9dbcb',
                           'secret' => '13dfb74249a8f7b6'});

my $response = $api->execute_method('flickr.photosets.getPhotos', {
                                                         'photoset_id' => '72157620417041389',
                                                         'baz' => 'quux',
                                                        });

foreach $key (keys %response) {
  $value = $response{$key};
  print "$key = $value \n";
}
