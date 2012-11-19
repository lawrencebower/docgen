package org.lawrencebower.docgen.web_logic.business.controler_business.product_selection;

import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactory;
import org.lawrencebower.docgen.web_logic.view.product.Product;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class ProductSelectionCB {

    @Autowired
    private ModelFactory modelFactory;

    public List<ProductView> getProducts() {
        return modelFactory.getProducts();
    }

    public Product getProduct(String productId) {
        return modelFactory.getProduct(productId);
    }
}
