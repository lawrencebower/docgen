package org.lawrencebower.docgen.web_model.view.product;

public class Product {

    private final String productId;
    private final String productName;

    public Product(String productId, String productName) {
        this.productId = productId;
        this.productName = productName;
    }

    public String getProductId() {
        return productId;
    }

    public String getProductName() {
        return productName;
    }
}
