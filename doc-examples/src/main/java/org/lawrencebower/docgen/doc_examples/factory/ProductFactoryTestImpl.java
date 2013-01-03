package org.lawrencebower.docgen.doc_examples.factory;

import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.product.ProductViewFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.ProductFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.LinkedHashMap;
import java.util.Map;

public class ProductFactoryTestImpl implements ProductFactory {

    @Autowired
    private ProductViewFactory productViewFactory;

    private final Map<String,ProductView> products = new LinkedHashMap<>();

    public static final String PRODUCT_ID_1 = "W1";
    public static final String PRODUCT_ID_2 = "W2";

    private void initProducts() {
        Product product1 = new Product(PRODUCT_ID_1,
                                       "Super Widget 1",
                                       "100.25",
                                       "UK",
                                       "(no laser, plastic)");

        Product product2 = new Product(PRODUCT_ID_2,
                                       "Mega Widget 2",
                                       "200",
                                       "UK",
                                       "(contains laser)",
                                       "84562574");

        ProductView productView1 = productViewFactory.createProductView(product1);
        ProductView productView2 = productViewFactory.createProductView(product2);

        products.put(productView1.getId(), productView1);
        products.put(productView2.getId(), productView2);
    }

    @Override
    public Map<String, ProductView> getProducts() {
        return products;
    }
}
