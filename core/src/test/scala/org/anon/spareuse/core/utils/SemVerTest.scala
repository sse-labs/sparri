package org.anon.spareuse.core.utils

import org.anon.spareuse.core.utils
import org.scalatest.funspec.AnyFunSpec

class SemVerTest extends AnyFunSpec {

  describe("Semantic version comparison") {

    it("should be equal for identical versions") {
      assert(utils.compareSemanticVersions("1.2.3", "1.2.3") === 0)
      assert(utils.compareSemanticVersions("2.0.0", "2.0.0") === 0)
      assert(utils.compareSemanticVersions("3.1.0-alpha", "3.1.0-alpha") === 0)
    }

    it("should handle major version differences") {
      assert(utils.compareSemanticVersions("1.2.3", "2.2.3") < 0)
      assert(utils.compareSemanticVersions("2.0.0", "1.0.0") > 0)
      assert(utils.compareSemanticVersions("3.1.0-alpha", "4.1.0-alpha") < 0)
    }

    it("should handle minor version differences") {
      assert(utils.compareSemanticVersions("1.2.3", "1.3.3") < 0)
      assert(utils.compareSemanticVersions("2.0.0", "2.1.0") < 0)
      assert(utils.compareSemanticVersions("3.1.0-alpha", "3.2.0-alpha") < 0)
    }

    it("should handle patch version differences") {
      assert(utils.compareSemanticVersions("1.2.3", "1.2.4") < 0)
      assert(utils.compareSemanticVersions("2.0.0", "2.0.1") < 0)
      assert(utils.compareSemanticVersions("3.1.0-alpha", "3.1.1-alpha") < 0)
    }

    it("should handle pre-release versions") {
      assert(utils.compareSemanticVersions("1.2.3-alpha", "1.2.3-beta") < 0)
      assert(utils.compareSemanticVersions("2.0.0-beta", "2.0.0-rc") < 0)
      assert(utils.compareSemanticVersions("3.1.0-alpha", "3.1.0-beta") < 0)
    }

    it("should ignore build metadata") {
      assert(utils.compareSemanticVersions("1.2.3+build123", "1.2.3+build456") === 0)
      assert(utils.compareSemanticVersions("2.0.0+alpha", "2.0.0+beta") === 0)
      assert(utils.compareSemanticVersions("3.1.0-alpha+123", "3.1.0-alpha+456") === 0)
    }
  }
}