package org.lawrencebower.docgen.web_model.view.product;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.constants.ViewConstants;

public class ProductView {

    private Product product;
    private int quantity = 1;

    private ProductView() {
    }

    public void setProduct(Product product) {
        this.product = product;
    }

    public String getId() {
        return product.getProductId();
    }

    public Product getProduct() {
        return product;
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

    public void incrementQuantity() {
        quantity++;
    }

    public void decrementQuantity() {

        if (quantity == 0) {
            String productId = product.getProductId();
            String template = "product %s quantity already zero?!";
            String message = String.format(template, productId);
            throw new DocGenException(message);
        }

        quantity--;
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
        return ViewConstants.PRODUCT_QUANTITY_TOKEN;
    }

    public String getCostToken() {
        return ViewConstants.PRODUCT_COST_TOKEN;
    }

    @Override
    public boolean equals(Object obj) {

        boolean isEqual = false;

        if ((obj instanceof ProductView)) {

            ProductView compareTo = (ProductView) obj;
            EqualsBuilder builder = new EqualsBuilder();
            String thisId = getId();
            String compareToId = compareTo.getId();
            builder.append(thisId, compareToId);

            isEqual = builder.isEquals();
        }

        return isEqual;
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        String id = getId();
        builder.append(id);
        return builder.toHashCode();
    }
}
