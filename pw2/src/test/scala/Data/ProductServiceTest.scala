package Data

import munit.ScalaCheckSuite

class ProductServiceTest extends ScalaCheckSuite {
  val productSvc: ProductService = new ProductImpl()

  test("getPrice should return the correct price for a known product and brand") {
    assertEquals(productSvc.getPrice("biere", "boxer"), 1.00)
    assertEquals(productSvc.getPrice("biere", "punkipa"), 3.00)
    assertEquals(productSvc.getPrice("croissant", "cailler"), 2.00)
  }

  test("getPrice should throw an exception for an unknown product") {
    intercept[NoSuchElementException] {
      productSvc.getPrice("unknown_product", "any_brand")
    }
  }

  test("getPrice should throw an exception for an unknown brand") {
    intercept[NoSuchElementException] {
      productSvc.getPrice("croissant", "any_brand")
    }
  }

  test("getDefaultBrand should return the correct default brand") {
    assertEquals(productSvc.getDefaultBrand("biere"), "boxer")
    assertEquals(productSvc.getDefaultBrand("croissant"), "maison")
  }

  test("getDefaultBrand should throw an exception for an unknown product") {
    intercept[NoSuchElementException] {
      productSvc.getDefaultBrand("unknown_product")
    }
  }

  test("products should return all available products and brands") {
    val expected = Map(
      "biere" -> List("boxer", "farmer", "wittekop", "punkipa", "jackhammer", "tenebreuse"),
      "croissant" -> List("maison", "cailler")
    )
    assertEquals(productSvc.products, expected)
  }
}
