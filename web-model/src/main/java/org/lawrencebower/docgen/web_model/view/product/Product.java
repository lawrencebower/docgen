package org.lawrencebower.docgen.web_model.view.product;

public class Product {

    private final String productId;
    private final String productName;
    private String value;
    private final String countryOfOrigin;
    private final String harmonizedTariffNumber;
    private final String customsDescription;

    public Product(String productId,
                   String productName,
                   String value,
                   String countryOfOrigin,
                   String customsDescription) {

        this(productId,
             productName,
             value,
             countryOfOrigin,
             customsDescription,
             "");
    }

    public Product(String productId,
                   String productName,
                   String value,
                   String countryOfOrigin,
                   String customsDescription,
                   String harmonizedTariffNumber) {

        this.productId = productId;
        this.productName = productName;
        this.value = value;
        this.countryOfOrigin = countryOfOrigin;
        this.customsDescription = customsDescription;
        this.harmonizedTariffNumber = harmonizedTariffNumber;
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

    public void setValue(String value) {
        this.value = value;
    }

    public String getCountryOfOrigin() {
        return countryOfOrigin;
    }

    public String getHarmonizedTariffNumber() {
        return harmonizedTariffNumber;
    }

    public String getCustomsDescription() {
        return customsDescription;
    }

}
