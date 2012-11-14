package org.lawrencebower.docgen.web_logic.business.product_injection;

import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public class ProductInjector {

    public void injectProductFields(List<DocComponentView> components,
                                    List<ProductView> products) {

        for (DocComponentView component : components) {
            injectProductIfSupported(component, products);
        }

    }

    private void injectProductIfSupported(DocComponentView component,
                                          List<ProductView> products) {

        if(component.allowsProductInjection()){
            component.injectProducts(products);
        }
    }
}
