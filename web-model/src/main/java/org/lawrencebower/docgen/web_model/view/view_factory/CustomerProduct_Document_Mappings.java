package org.lawrencebower.docgen.web_model.view.view_factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class CustomerProduct_Document_Mappings {

    /**
     * using linked map as its helpful to preserve the order for tests
     */
    protected Map<CustomerProductPair, List<String>> mappings = new LinkedHashMap<>();

    public void addDocument(ContactView customer,
                            ProductView product,
                            String documentName) {

        CustomerProductPair pair = makeCustomerProductPair(customer, product);

        addDocument(pair, documentName);
    }

    public void addDocument(CustomerProductPair pair, String documentName) {
        if (mappings.containsKey(pair)) {
            List<String> documentViews = mappings.get(pair);
            documentViews.add(documentName);
        }else{
            ArrayList<String> list = new ArrayList<>();
            list.add(documentName);
            mappings.put(pair, list);
        }
    }

    public List<String> getDocumentsForCustomerAndProduct(ContactView business, ProductView product){

        List<String> results = new ArrayList<>();

        CustomerProductPair customerProductPair = makeCustomerProductPair(business, product);

        if(mappings.containsKey(customerProductPair)){
            List<String> documentNames = mappings.get(customerProductPair);
            for (String documentName : documentNames) {
                results.add(documentName);
            }
        }

        return results;
    }

    private CustomerProductPair makeCustomerProductPair(ContactView customer, ProductView product) {
        return new CustomerProductPair(customer, product);
    }

}
