package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactBuilder;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.ContactViewFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataRow;
import org.springframework.beans.factory.annotation.Autowired;

public class ContactMapper {

    @Autowired
    private ContactViewFactory viewFactory;

    public ContactView mapCustomerInfo(DataRow dataRow) {

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
        builder.setName(name);
        builder.setContactName(contactName);
        builder.setAddress(address);
        builder.setPhone(phone);
        builder.setCountry(country);
        builder.setAttributes(attributes);
        builder.setTaxId(taxId);
        builder.setEmail(email);

        Contact contact = builder.buildContact();

        return viewFactory.createContactView(contact, id);
    }

}
