package org.lawrencebower.docgen.web.controller.product_selection;

public class ProductSelectionBean {
    private String productId;

    public ProductSelectionBean() {
    }

    public ProductSelectionBean(String productId) {
        this.productId = productId;
    }

    public String getProductId() {
        return productId;
    }

    public void setProductId(String productId) {
        this.productId = productId;
    }
}
