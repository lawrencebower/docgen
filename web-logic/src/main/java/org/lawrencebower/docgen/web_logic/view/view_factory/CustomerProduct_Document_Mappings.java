package org.lawrencebower.docgen.web_logic.view.view_factory;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

class CustomerProduct_Document_Mappings {

    /**
     * using linked map as its helpful to preserve the order for tests
     */
    protected Map<CustomerProductPair, List<String>> mappings = new LinkedHashMap<>();

    protected void addDocument(String customerName,
                            String productId,
                            String documentName) {

        CustomerProductPair pair = makeCustomerProductPair(customerName, productId);

        if (mappings.containsKey(pair)) {
            List<String> documentViews = mappings.get(pair);
            documentViews.add(documentName);
        }else{
            ArrayList<String> list = new ArrayList<>();
            list.add(documentName);
            mappings.put(pair, list);
        }
    }

    protected List<String> getDocumentsForCustomerAndProduct(ContactView business, ProductView product){

        List<String> results = new ArrayList<>();

        String businessName = business.getName();
        String productId = product.getProductId();
        CustomerProductPair customerProductPair = makeCustomerProductPair(businessName, productId);

        if(mappings.containsKey(customerProductPair)){
            List<String> documentNames = mappings.get(customerProductPair);
            for (String documentName : documentNames) {
                results.add(documentName);
            }
        }

        return results;
    }

    private CustomerProductPair makeCustomerProductPair(String customerName, String productId) {
        return new CustomerProductPair(customerName, productId);
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
