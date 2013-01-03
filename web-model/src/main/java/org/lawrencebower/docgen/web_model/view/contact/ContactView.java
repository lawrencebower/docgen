package org.lawrencebower.docgen.web_model.view.contact;

import org.lawrencebower.docgen.web_model.business_def.utils.ViewUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class ContactView {

    @Autowired
    private ViewUtils viewUtils;

    private Contact contact;

    private ContactView() {//force spring creation
    }

    public void setContact(Contact contact) {
        this.contact = contact;
    }

    public String getName(){
        return contact.getName();
    }

    public String getContactName(){
        return contact.getContactName();
    }

    public String getAddress(){
        return contact.getAddress();
    }

    public String getHTMLAddress(){
        String address = contact.getAddress();
        return viewUtils.toHTMLString(address);
    }

    public Contact getContact() {
        return contact;
    }

    public String getCountry() {
        return contact.getCountry();
    }

    public String getPhone() {
        return contact.getPhone();
    }

    public String getEmail() {
        return contact.getEmail();
    }

    public String getTaxId() {
        return contact.getTaxId();
    }
}
