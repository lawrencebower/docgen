package org.lawrencebower.docgen.web_model.view.product;

public class ProductView {

    private Product product;

    public ProductView(Product product) {
        this.product = product;
    }

    public String getId() {
        return product.getProductId();
    }

    public Product getProduct() {
        return product;
    }
}
