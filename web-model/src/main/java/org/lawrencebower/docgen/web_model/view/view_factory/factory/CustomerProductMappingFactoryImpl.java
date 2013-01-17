package org.lawrencebower.docgen.web_model.view.view_factory.factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.CustomerProductPair;
import org.lawrencebower.docgen.web_model.view.view_factory.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_model.view.view_factory.CustomerProduct_Document_MappingsImpl;

import java.util.ArrayList;
import java.util.List;

public class CustomerProductMappingFactoryImpl implements CustomerProductMappingFactory {

    private CustomerProduct_Document_Mappings customerProductDocMappings;

    @Override
    public void initMappingInfo(List<ContactView> customers,
                                List<ProductView> products,
                                List<DocumentView> documents) {

        List<CustomerProductPair> customerProductPairs = makeCustomerProductPairs(customers, products);

        customerProductDocMappings = makeMappings(customerProductPairs, documents);
    }

    @Override
    public List<String> getDocumentsForCustomerAndProduct(ContactView customer, ProductView product) {
        return customerProductDocMappings.getDocumentsForCustomerAndProduct(customer, product);
    }

    private CustomerProduct_Document_Mappings makeMappings(List<CustomerProductPair> customerProductPairs,
                                                           List<DocumentView> documents) {

        CustomerProduct_Document_Mappings mappings = new CustomerProduct_Document_MappingsImpl();

        for (CustomerProductPair customerProductPair : customerProductPairs) {
            addDocumentsWhereMatch(documents,
                                   mappings,
                                   customerProductPair);
        }


        return mappings;
    }

    private void addDocumentsWhereMatch(List<DocumentView> documents,
                                        CustomerProduct_Document_Mappings mappings,
                                        CustomerProductPair customerProductPair) {

        for (DocumentView document : documents) {

            boolean documentMatch = customerProductPair.doesDocumentMatch(document);

            if (documentMatch) {
                String documentName = document.getName();
                mappings.addDocument(customerProductPair, documentName);
            }
        }
    }

    private List<CustomerProductPair> makeCustomerProductPairs(List<ContactView> customers,
                                                               List<ProductView> products) {

        List<CustomerProductPair> allPairs = new ArrayList<>();

        for (ContactView customer : customers) {
            List<CustomerProductPair> customerPairs = makePairsForAllProducts(customer, products);
            allPairs.addAll(customerPairs);
        }

        return allPairs;
    }

    private List<CustomerProductPair> makePairsForAllProducts(ContactView customer,
                                                              List<ProductView> products) {

        List<CustomerProductPair> pairs = new ArrayList<>();

        for (ProductView product : products) {
            pairs.add(new CustomerProductPair(customer, product));
        }

        return pairs;
    }
}
