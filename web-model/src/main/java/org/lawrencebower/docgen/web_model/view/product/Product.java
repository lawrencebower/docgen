package org.lawrencebower.docgen.web_model.view.product;

import org.lawrencebower.docgen.web_model.view.view_factory.Attributes;

public class Product {

    private String productId;
    private String productName;
    private String modelNumber;
    private String value;
    private String countryOfOrigin;
    private String harmonizedTariffNumber;
    private String customsDescription;
    private String tradeName;
    private Attributes attributes = new Attributes();

    protected Product(String productId) {//only make with a product builder
        this.productId = productId;
    }

    public void setModelNumber(String modelNumber) {
        this.modelNumber = modelNumber;
    }

    protected void setProductName(String productName) {
        this.productName = productName;
    }

    protected void setValue(String value) {
        this.value = value;
    }

    protected void setCountryOfOrigin(String countryOfOrigin) {
        this.countryOfOrigin = countryOfOrigin;
    }

    protected void setHarmonizedTariffNumber(String harmonizedTariffNumber) {
        this.harmonizedTariffNumber = harmonizedTariffNumber;
    }

    protected void setCustomsDescription(String customsDescription) {
        this.customsDescription = customsDescription;
    }

    protected void setTradeName(String tradeName) {
        this.tradeName = tradeName;
    }

    protected void setAttributes(String... attributes) {
        this.attributes = new Attributes(attributes);
    }

    public String getProductId() {
        return productId;
    }

    public String getModelNumber() {
        return modelNumber;
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

    public String getHarmonizedTariffNumber() {
        return harmonizedTariffNumber;
    }

    public String getCustomsDescription() {
        return customsDescription;
    }

    public String getTradeName() {
        return tradeName;
    }

    public Attributes getAttributes() {
        return attributes;
    }

    public boolean isAttributesMatch(Attributes attributes) {
        return this.attributes.isAttributeMatch(attributes);
    }
}
