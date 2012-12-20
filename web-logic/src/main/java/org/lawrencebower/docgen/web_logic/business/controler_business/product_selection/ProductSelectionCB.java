package org.lawrencebower.docgen.web_logic.business.controler_business.product_selection;

import org.lawrencebower.docgen.web_logic.view.model_factory.ViewFactory;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class ProductSelectionCB {

    @Autowired
    private ViewFactory viewFactory;

    public List<ProductView> getProducts() {
        return viewFactory.getProducts();
    }

    public ProductView getProduct(String productId) {
        return viewFactory.getProduct(productId);
    }
}
