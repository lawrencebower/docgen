package org.lawrencebower.docgen.web_logic.view.product;

public class Product {

    private final String productId;
    private final String productName;
    private final String value;
    private final String countryOfOrigin;

    public Product(String productId,
                   String productName,
                   String value,
                   String countryOfOrigin) {

        this.productId = productId;
        this.productName = productName;
        this.value = value;
        this.countryOfOrigin = countryOfOrigin;
    }

    public String getProductId() {
        return productId;
    }

    public String getProductName() {
        return productName;
    }

    public String getValue() {
        return value;
    }

    public String getCountryOfOrigin() {
        return countryOfOrigin;
    }
}
