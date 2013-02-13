package org.lawrencebower.docgen.web_model.view.view_factory.factory;

import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;
import java.util.Map;

public interface ProductFactory {

    Map<String,Product> getProducts();

    List<ProductView> getProductsAsList();

    boolean hasProduct(String productId);

    ProductView getProduct(String productId);

    void reloadProducts();
}
