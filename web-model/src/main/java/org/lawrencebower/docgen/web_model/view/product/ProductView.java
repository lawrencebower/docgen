package org.lawrencebower.docgen.web_model.view.product;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_model.view.view_factory.Attributes;

public class ProductView {

    private Product product;
    private int quantity = 1;

    private ProductView() {
    }

    public void setProduct(Product product) {
        this.product = product;
    }

    public String getProductName() {
        return product.getProductName();
    }

    public String getProductId() {
        return product.getProductId();
    }

    public String getProductValue() {
        return product.getValue();
    }

    public String getProductCountryOfOrigin() {
        return product.getCountryOfOrigin();
    }

    public String getTradeName() {
        return product.getTradeName();
    }

    public void incrementQuantity() {
        quantity++;
    }

    public int getQuantity() {
        return quantity;
    }

    public String getQuantityString() {
        return Integer.toString(quantity);
    }

    public String getHarmonizedTariffNumber() {
        return product.getHarmonizedTariffNumber();
    }

    public String getCustomsDescription() {
        return product.getCustomsDescription();
    }

    public Attributes getShippingAttributes(){
        return product.getAttributes();
    }

    public String getCommercialInvoiceDescription() {

        StringBuilder builder = new StringBuilder();

        String productId = product.getProductId();
        String productName = product.getProductName();
        String customsDescription = product.getCustomsDescription();

        builder.append(productId);
        builder.append(" - ");
        builder.append(productName);
        builder.append(" ");
        builder.append(customsDescription);

        if (hasHarmonizedTariffNumber()) {
            String tariff = product.getHarmonizedTariffNumber();
            builder.append(" ");
            builder.append(tariff);
        }

        return builder.toString();
    }

    private boolean hasHarmonizedTariffNumber() {
        String number = getHarmonizedTariffNumber();
        return StringUtils.isNotBlank(number);
    }

    public String getFieldSeparator() {
        return ViewConstants.DOCUMENT_FIELD_SEPARATOR;
    }

    public String getQuantityToken() {
        return ViewConstants.PRODUCT_TOKEN_TYPE.QUANTITY.getName();
    }

    public String getValueToken() {
        return ViewConstants.PRODUCT_TOKEN_TYPE.VALUE.getName();
    }

    public void setValue(String valueString) {
        product.setValue(valueString);
    }

    public void setQuantity(String quantityString) {
        try {
            quantity = Integer.parseInt(quantityString);
        } catch (NumberFormatException e) {
            String message = String.format("Can not parse string '%s' into an integer", quantityString);
            throw new DocGenException(message);
        }
    }

    public boolean idMatches(String productId) {
        String thisId = getProductId();
        return productId.equals(thisId);
    }

    public boolean idDoesNotMatch(String productId) {
        return !idMatches(productId);
    }

    @Override
    public boolean equals(Object obj) {

        boolean isEqual = false;

        if ((obj instanceof ProductView)) {

            ProductView compareTo = (ProductView) obj;
            EqualsBuilder builder = new EqualsBuilder();
            String thisId = getProductId();
            String compareToId = compareTo.getProductId();
            builder.append(thisId, compareToId);

            isEqual = builder.isEquals();
        }

        return isEqual;
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        String id = getProductId();
        builder.append(id);
        return builder.toHashCode();
    }

    public boolean isAttributesMatch(Attributes attributes) {
        return product.isAttributesMatch(attributes);
    }

    public Attributes getAttributes() {
        return product.getAttributes();
    }
}
