package org.lawrencebower.docgen.web_model.view.view_factory.factory;

import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.Map;

public interface ProductFactory {

    Map<String, ProductView> getProducts();

}
