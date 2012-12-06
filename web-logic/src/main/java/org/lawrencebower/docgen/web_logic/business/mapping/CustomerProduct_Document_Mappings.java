package org.lawrencebower.docgen.web_logic.business.mapping;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactory;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.Product;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class CustomerProduct_Document_Mappings {

    @Autowired
    ModelFactory modelFactory;

    /**
     * using linked map as its helpful to preserve the order for tests
     */
    protected Map<CustomerProductPair, List<String>> mappings = new LinkedHashMap<>();

    public void addDocument(ContactView business,
                            Product product,
                            String documentName) {

        CustomerProductPair pair = makeCustomerProductPair(business, product);

        if (mappings.containsKey(pair)) {
            List<String> documentViews = mappings.get(pair);
            documentViews.add(documentName);
        }else{
            ArrayList<String> list = new ArrayList<>();
            list.add(documentName);
            mappings.put(pair, list);
        }
    }

    public List<DocumentView> getDocumentsForCustomerAndProduct(ContactView business, Product product){

        List<DocumentView> results = new ArrayList<>();

        CustomerProductPair customerProductPair = makeCustomerProductPair(business, product);
        if(mappings.containsKey(customerProductPair)){
            List<String> documentNames = mappings.get(customerProductPair);
            for (String documentName : documentNames) {
                DocumentView document = modelFactory.getDocument(documentName);
                results.add(document);
            }
        }

        return results;
    }

    private CustomerProductPair makeCustomerProductPair(ContactView business, Product product) {
        String name = business.getName();
        String productId = product.getProductId();
        return new CustomerProductPair(name, productId);
    }

    class CustomerProductPair {

        private String customerName;
        private String productId;

        CustomerProductPair(String customerName, String productId) {
            this.customerName = customerName;
            this.productId = productId;
        }

        @Override
        public boolean equals(Object obj) {

            boolean isEqual = false;
            if (obj instanceof CustomerProductPair) {
                CustomerProductPair compareTo = (CustomerProductPair) obj;

                EqualsBuilder builder = new EqualsBuilder();

                builder.append(this.customerName, compareTo.customerName);
                builder.append(this.productId, compareTo.productId);

                isEqual = builder.isEquals();
            }

            return isEqual;
        }

        @Override
        public int hashCode() {
            HashCodeBuilder builder = new HashCodeBuilder();

            builder.append(this.customerName);
            builder.append(this.productId);

            return builder.toHashCode();
        }
    }
}
