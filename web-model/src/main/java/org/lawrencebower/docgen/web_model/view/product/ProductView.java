package org.lawrencebower.docgen.web_model.view.product;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.exception.DocGenException;

public class ProductView {

    private Product product;
    private int quantity = 1;

    public ProductView(Product product) {
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

    public String getProductValue(){
        return product.getValue();
    }

    public String getProductCountryOfOrigin(){
        return product.getCountryOfOrigin();
    }

    public void incrementQuantity(){
        quantity++;
    }

    public void decrementQuantity(){

        if(quantity == 0){
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

    @Override
    public boolean equals(Object obj) {

        if(!(obj instanceof ProductView)){
            return false;
        }

        ProductView compareTo = (ProductView) obj;
        EqualsBuilder builder = new EqualsBuilder();
        builder.append(this.getId(), compareTo.getId());

        return builder.isEquals();
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(this.getId());
        return builder.toHashCode();
    }
}
