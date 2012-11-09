package org.lawrencebower.docgen.web_logic.business.mapping;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.Product;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CustomerProduct_Document_Mappings {

    private Map<CustomerProductPair, List<DocumentInfoView>> mappings = new HashMap<>();

    public void addDocument(Contact business,
                            Product product,
                            DocumentInfoView document) {

        CustomerProductPair pair = makeCustomerProductPair(business, product);

        if (mappings.containsKey(pair)) {
            List<DocumentInfoView> documentInfoViews = mappings.get(pair);
            documentInfoViews.add(document);
        }else{
            ArrayList<DocumentInfoView> list = new ArrayList<>();
            list.add(document);
            mappings.put(pair, list);
        }
    }

    public List<DocumentInfoView> getDocInfosForCustomerAndProduct(Contact business, Product product){

        List<DocumentInfoView> results = new ArrayList<>();

        CustomerProductPair customerProductPair = makeCustomerProductPair(business, product);
        if(mappings.containsKey(customerProductPair)){
            List<DocumentInfoView> docInfos = mappings.get(customerProductPair);
            results.addAll(docInfos);
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
