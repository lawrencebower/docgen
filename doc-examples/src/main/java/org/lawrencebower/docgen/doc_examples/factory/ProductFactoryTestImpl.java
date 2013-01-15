package org.lawrencebower.docgen.doc_examples.factory;

import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductBuilder;
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

        ProductBuilder builder = new ProductBuilder();
        builder.setProductId(PRODUCT_ID_1);
        builder.setProductName("Super Widget 1");
        builder.setValue("100.25");
        builder.setCountryOfOrigin("UK");
        builder.setCustomsDescription("(no laser, plastic)");

        Product product1 = builder.buildProduct();

        builder = new ProductBuilder();
        builder.setProductId(PRODUCT_ID_2);
        builder.setProductName("Mega Widget 2");
        builder.setValue("200");
        builder.setCountryOfOrigin("UK");
        builder.setCustomsDescription("(contains laser)");
        builder.setHarmonizedTariffNumber("84562574");

        Product product2 = builder.buildProduct();

        ProductView productView1 = productViewFactory.createProductView(product1);
        ProductView productView2 = productViewFactory.createProductView(product2);

        products.put(productView1.getProductId(), productView1);
        products.put(productView2.getProductId(), productView2);
    }

    @Override
    public Map<String, ProductView> getProducts() {
        return products;
    }
}
