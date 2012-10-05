package org.lawrencebower.docgen.web_logic.view.product;

import org.lawrencebower.docgen.web_logic.model.product.Product;

public class ProductView {

    private Product product;

    public ProductView(Product product) {
        this.product = product;
    }

    public String getId() {
        return product.getProductId();
    }

    public Product getproduct() {
        return product;
    }
}
