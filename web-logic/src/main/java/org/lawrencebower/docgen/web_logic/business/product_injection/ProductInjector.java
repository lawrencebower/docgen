package org.lawrencebower.docgen.web_logic.business.product_injection;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.List;

public interface ProductInjector<T extends DocComponent> {
    void injectProducts(T component, List<ProductView> products);
}
