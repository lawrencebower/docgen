package org.lawrencebower.docgen.web_model.view.view_factory.factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public interface CustomerProductMappingFactory {

    void initMappingInfo(List<ContactView> customers,
                         List<ProductView> products,
                         List<DocumentView> documents);

    List<String> getDocumentsForCustomerAndProduct(ContactView customer,
                                                   ProductView product);
}
