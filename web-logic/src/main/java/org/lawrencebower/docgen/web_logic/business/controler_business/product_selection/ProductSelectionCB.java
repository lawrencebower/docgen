package org.lawrencebower.docgen.web_logic.business.controler_business.product_selection;

import org.lawrencebower.docgen.web_model.view.product.ProductSelection;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.Map;

public class ProductSelectionCB {

    @Autowired(required = false)
    private ViewFactory viewFactory;

    public List<ProductView> getProducts() {
        return viewFactory.getProducts();
    }

    public ProductView getProduct(String productId) {
        return viewFactory.getProduct(productId);
    }

    public void mapFieldValuesToProducts(Map<String, String[]> parameterMap,
                                         ProductSelection productSelection) {

        productSelection.mapFieldValuesToComponents(parameterMap);
    }

}
