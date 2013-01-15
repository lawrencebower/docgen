package org.lawrencebower.docgen.web_model.view.view_factory;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

public class CustomerProductPair {

    private ContactView customer;
    private ProductView product;

    public CustomerProductPair(ContactView customer,
                               ProductView product) {

        this.customer = customer;
        this.product = product;
    }

    public boolean doesDocumentMatch(DocumentView document) {

        boolean regionMatch = document.isContactAttributesMatch(customer);

        boolean productMatch = document.isProductAttributesMatch(product);

        return regionMatch && productMatch;
    }

    @Override
    public boolean equals(Object obj) {

        boolean isEqual = false;
        if (obj instanceof CustomerProductPair) {
            CustomerProductPair compareTo = (CustomerProductPair) obj;

            EqualsBuilder builder = new EqualsBuilder();

            String thisContactId = customer.getContactId();
            String compareToContactId = compareTo.customer.getContactId();

            String thisProductId = product.getProductId();
            String compareToProductId = compareTo.product.getProductId();

            builder.append(thisContactId, compareToContactId);
            builder.append(thisProductId, compareToProductId);

            isEqual = builder.isEquals();
        }

        return isEqual;
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();

        String contactId = customer.getContactId();
        String productId = product.getProductId();

        builder.append(contactId);
        builder.append(productId);

        return builder.toHashCode();
    }
}
