package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactBuilder;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataRow;

public class ContactMapper {

    public Contact mapCustomerInfo(DataRow dataRow) {

        String id = dataRow.getString(0);
        String name = dataRow.getString(1);
        String contactName = dataRow.getString(2);
        String address = dataRow.getString(3);
        String phone = dataRow.getString(4);
        String country = dataRow.getString(5);
        String[] attributes = dataRow.getStringArray(6);
        String taxId = dataRow.getString(7);
        String email = dataRow.getString(8);

        ContactBuilder builder = new ContactBuilder();
        builder.setContactId(id);
        builder.setName(name);
        builder.setContactName(contactName);
        builder.setAddress(address);
        builder.setPhone(phone);
        builder.setCountry(country);
        builder.setAttributes(attributes);
        builder.setTaxId(taxId);
        builder.setEmail(email);

        return builder.buildContact();
    }

}
