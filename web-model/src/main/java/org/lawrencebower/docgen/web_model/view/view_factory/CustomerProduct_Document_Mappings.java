package org.lawrencebower.docgen.web_model.view.view_factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public interface CustomerProduct_Document_Mappings {

    void addDocument(ContactView customer,
                     ProductView product,
                     String documentName);

    void addDocument(CustomerProductPair pair,
                     String documentName);

    List<String> getDocumentsForCustomerAndProduct(ContactView customer,
                                                   ProductView product);
}
