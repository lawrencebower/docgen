package org.lawrencebower.docgen.web_logic.business.controler_business.product_selection;

import org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.product.ProductMapper;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.Map;

public class ProductSelectionCB {

    @Autowired
    private ViewFactory viewFactory;
    @Autowired
    private ProductMapper productMapper;

    public List<ProductView> getProducts() {
        return viewFactory.getProducts();
    }

    public ProductView getProduct(String productId) {
        return viewFactory.getProduct(productId);
    }

    public void mapFieldValuesToProducts(Map<String, String[]> parameterMap,
                                         List<ProductView> products) {

        productMapper.mapFieldValuesToComponents(parameterMap, products);
    }

}
