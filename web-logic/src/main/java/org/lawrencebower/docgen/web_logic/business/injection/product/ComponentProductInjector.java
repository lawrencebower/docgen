package org.lawrencebower.docgen.web_logic.business.injection.product;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public interface ComponentProductInjector<T extends DocComponent> {
    void injectProducts(T component, List<ProductView> products);
}
