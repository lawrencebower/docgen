package org.lawrencebower.docgen.web_logic.business.mapping;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.web_logic.model.customer.Customer;
import org.lawrencebower.docgen.web_logic.model.product.Product;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CustomerProduct_Document_Mappings {

    private Map<CustomerProductPair, List<DocumentInfo>> mappings = new HashMap<>();

    public void addDocument(Customer customer,
                            Product product,
                            DocumentInfo document) {

        CustomerProductPair pair = makeCustomerProductPair(customer, product);

        if (mappings.containsKey(pair)) {
            mappings.get(pair).add(document);
        }else{
            ArrayList<DocumentInfo> list = new ArrayList<>();
            list.add(document);
            mappings.put(pair, list);
        }
    }

    private CustomerProductPair makeCustomerProductPair(Customer customer, Product product) {
        return new CustomerProductPair(customer.getName(), product.getProductId());
    }

    public List<DocumentInfo> getDocInfosForCustomerAndProduct(Customer customer, Product product){

        CustomerProductPair customerProductPair = makeCustomerProductPair(customer, product);
        if(mappings.containsKey(customerProductPair)){
            return mappings.get(customerProductPair);
        }

        return new ArrayList<>();
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

            if (obj instanceof CustomerProductPair) {
                CustomerProductPair compareTo = (CustomerProductPair) obj;

                EqualsBuilder builder = new EqualsBuilder();

                builder.append(this.customerName, compareTo.customerName);
                builder.append(this.productId, compareTo.productId);

                return builder.isEquals();
            }

            return false;
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
