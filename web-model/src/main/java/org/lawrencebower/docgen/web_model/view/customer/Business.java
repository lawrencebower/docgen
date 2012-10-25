package org.lawrencebower.docgen.web_model.view.customer;

public class Business {

    private String name;
    private String contactName;
    private String phone;
    private String country;
    private String address;

    public Business(String name,
                    String contactName,
                    String address,
                    String phone,
                    String country) {

        this.name = name;
        this.contactName = contactName;
        this.address = address;
        this.phone = phone;
        this.country = country;
    }

    public String getName() {
        return name;
    }

    public String getAddress() {
        return address;
    }

    public String getCountry() {
        return country;
    }

    public String getPhone() {
        return phone;
    }

    public String getContactName() {
        return contactName;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setContactName(String contactName) {
        this.contactName = contactName;
    }

    public void setPhone(String phone) {
        this.phone = phone;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    public void setAddress(String address) {
        this.address = address;
    }
}
