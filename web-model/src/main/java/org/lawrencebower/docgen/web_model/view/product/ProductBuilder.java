package org.lawrencebower.docgen.web_model.view.product;

public class ProductBuilder {

    private Product product;

    public ProductBuilder(String productId) {
        product = new Product(productId);
    }

    public void setProductName(String productName) {
        product.setProductName(productName);
    }

    public void setModelNumber(String modelNumber) {
        product.setModelNumber(modelNumber);
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

    public void setProductDescription(String productDescription) {
        product.setProductDescription(productDescription);
    }

    public void setAttributes(String... attributes) {
        product.setAttributes(attributes);
    }

    public void setTradeName(String tradeName) {
        product.setTradeName(tradeName);
    }

    public Product buildProduct() {
        checkRequiredFields();
        return product;
    }

    private void checkRequiredFields() {
        //todo
    }

}
