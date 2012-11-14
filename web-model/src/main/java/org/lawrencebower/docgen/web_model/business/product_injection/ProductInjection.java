package org.lawrencebower.docgen.web_model.business.product_injection;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public interface ProductInjection<T extends DocComponent> {
   void injectProducts(T component, List<ProductView> products);
}
