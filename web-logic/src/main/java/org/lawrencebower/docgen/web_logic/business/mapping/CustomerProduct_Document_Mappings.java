package org.lawrencebower.docgen.web_logic.business.mapping;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.web_logic.view.contact.Contact;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.Product;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class CustomerProduct_Document_Mappings {

    /**
     * using linked map as its helpful to preserve the order for tests
     */
    protected Map<CustomerProductPair, List<DocumentView>> mappings = new LinkedHashMap<>();

    public void addDocument(Contact business,
                            Product product,
                            DocumentView document) {

        CustomerProductPair pair = makeCustomerProductPair(business, product);

        if (mappings.containsKey(pair)) {
            List<DocumentView> documentViews = mappings.get(pair);
            documentViews.add(document);
        }else{
            ArrayList<DocumentView> list = new ArrayList<>();
            list.add(document);
            mappings.put(pair, list);
        }
    }

    public List<DocumentView> getDocumentsForCustomerAndProduct(Contact business, Product product){

        List<DocumentView> results = new ArrayList<>();

        CustomerProductPair customerProductPair = makeCustomerProductPair(business, product);
        if(mappings.containsKey(customerProductPair)){
            List<DocumentView> documentViews = mappings.get(customerProductPair);
            results.addAll(documentViews);
        }

        return results;
    }

    private CustomerProductPair makeCustomerProductPair(Contact business, Product product) {
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
