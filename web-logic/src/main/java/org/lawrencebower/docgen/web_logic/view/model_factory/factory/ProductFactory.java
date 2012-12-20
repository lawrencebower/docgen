package org.lawrencebower.docgen.web_logic.view.model_factory.factory;

import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.Map;

public interface ProductFactory {

    Map<String, ProductView> getProducts();

}
