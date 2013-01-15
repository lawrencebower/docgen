package org.lawrencebower.docgen.web_model.view.product;

public class ProductBuilder {

    private Product product;

    public ProductBuilder() {
        product = new Product();
    }

    public void setProductId(String productId) {
        product.setProductId(productId);
    }

    public void setProductName(String productName) {
        product.setProductName(productName);
    }

    public void setValue(String value){
        product.setValue(value);
    }

    public void setCountryOfOrigin(String countryOfOrigin) {
        product.setCountryOfOrigin(countryOfOrigin);
    }

    public void setHarmonizedTariffNumber(String harmonizedTariffNumber) {
        product.setHarmonizedTariffNumber(harmonizedTariffNumber);
    }

    public void setCustomsDescription(String customsDescription) {
        product.setCustomsDescription(customsDescription);
    }

    public void setAttributes(String... attributes) {
        product.setAttributes(attributes);
    }

    public Product buildProduct() {
        return product;
    }
}
