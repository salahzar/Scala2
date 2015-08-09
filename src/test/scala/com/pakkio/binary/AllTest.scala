package com.pakkio.binary

import org.scalatest.Suites

class AllTest extends Suites (
  new BufferTest,
  new Conversions,
  new DecipherTest
)
