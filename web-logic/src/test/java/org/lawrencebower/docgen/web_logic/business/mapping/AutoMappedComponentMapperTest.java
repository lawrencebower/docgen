package org.lawrencebower.docgen.web_logic.business.mapping;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_logic.view.contact.Contact;
import org.lawrencebower.docgen.web_logic.view.document_info.DocComponentView;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Arrays;
import java.util.List;

import static org.mockito.Mockito.*;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class AutoMappedComponentMapperTest {

    @Autowired
    AutoMappedComponentMapper mapper;

    @Mock
    Contact customer;
    @Mock
    Contact business;
    @Mock
    Contact vendor;

    public static final String BUSINESS_NAME_STRING = "businessname";
    public static final String BUSINESS_CONTACT_NAME_STRING = "businesscontact";
    public static final String BUSINESS_PHONE_STRING = "businessphone";
    public static final String BUSINESS_COUNTRY_STRING = "businesscountry";
    public static final String BUSINESS_ADDRESS_STRING = "businesaddress";

    public static final String VENDOR_NAME_STRING = "vendorname";
    public static final String VENDOR_CONTACT_NAME_STRING = "vendorcontact";
    public static final String VENDOR_PHONE_STRING = "vendorphone";
    public static final String VENDOR_COUNTRY_STRING = "vendorcountry";
    public static final String VENDOR_ADDRESS_STRING = "vendoraddress";
    public static final String VENDOR_EMAIL_STRING = "vendoremail";
    public static final String VENDOR_TAXID_STRING = "vendortaxid";

    public static final String CUSTOMER_NAME_STRING = "customername";
    public static final String CUSTOMER_CONTACT_NAME_STRING = "customercontact";
    public static final String CUSTOMER_PHONE_STRING = "customerphone";
    public static final String CUSTOMER_COUNTRY_STRING = "customercountry";
    public static final String CUSTOMER_ADDRESS_STRING = "customeraddress";

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        programMocks();
    }

    private void programMocks() {

        customer = mock(Contact.class);
        when(customer.getName()).thenReturn(CUSTOMER_NAME_STRING);
        when(customer.getContactName()).thenReturn(CUSTOMER_CONTACT_NAME_STRING);
        when(customer.getPhone()).thenReturn(CUSTOMER_PHONE_STRING);
        when(customer.getCountry()).thenReturn(CUSTOMER_COUNTRY_STRING);
        when(customer.getAddress()).thenReturn(CUSTOMER_ADDRESS_STRING);

        business = mock(Contact.class);
        when(business.getName()).thenReturn(BUSINESS_NAME_STRING);
        when(business.getContactName()).thenReturn(BUSINESS_CONTACT_NAME_STRING);
        when(business.getPhone()).thenReturn(BUSINESS_PHONE_STRING);
        when(business.getCountry()).thenReturn(BUSINESS_COUNTRY_STRING);
        when(business.getAddress()).thenReturn(BUSINESS_ADDRESS_STRING);

        vendor = mock(Contact.class);
        when(vendor.getName()).thenReturn(VENDOR_NAME_STRING);
        when(vendor.getContactName()).thenReturn(VENDOR_CONTACT_NAME_STRING);
        when(vendor.getPhone()).thenReturn(VENDOR_PHONE_STRING);
        when(vendor.getCountry()).thenReturn(VENDOR_COUNTRY_STRING);
        when(vendor.getAddress()).thenReturn(VENDOR_ADDRESS_STRING);
        when(vendor.getEmail()).thenReturn(VENDOR_EMAIL_STRING);
        when(vendor.getTaxId()).thenReturn(VENDOR_TAXID_STRING);
    }

    //GENERAL TESTS//

    @Test
    public void testMapFields_automappedField_correctFieldMapped() {

        DocComponentView businessNameComponent = mockDocComponent();
        when(businessNameComponent.getAutoMappedComponent()).thenReturn(true);
        when(businessNameComponent.getAutoMappedField()).thenReturn(AutoMappedComponent.BUSINESS_NAME);

        DocComponentView nonAutomappedComponent = mockDocComponent();
        when(nonAutomappedComponent.getAutoMappedComponent()).thenReturn(false);

        List<DocComponentView> components = Arrays.asList(businessNameComponent, nonAutomappedComponent);

        mapper.mapFields(components,
                         customer,
                         vendor,
                         business);

        verify(businessNameComponent, atLeastOnce()).getAutoMappedComponent();
        verify(nonAutomappedComponent, atLeastOnce()).getAutoMappedComponent();
        verify(businessNameComponent, times(1)).setComponentValue(BUSINESS_NAME_STRING);
        verify(nonAutomappedComponent, never()).setComponentValue(anyString());
    }

    @Test
    public void testMapFields_noAutomappedField_noFieldMapped() {

        DocComponentView businessNameComponent = mockDocComponent();
        when(businessNameComponent.getAutoMappedComponent()).thenReturn(false);

        DocComponentView nonAutomappedComponent = mockDocComponent();
        when(nonAutomappedComponent.getAutoMappedComponent()).thenReturn(false);

        List<DocComponentView> components = Arrays.asList(businessNameComponent, nonAutomappedComponent);

        mapper.mapFields(components,
                         customer,
                         vendor,
                         business);

        verify(businessNameComponent, atLeastOnce()).getAutoMappedComponent();
        verify(nonAutomappedComponent, atLeastOnce()).getAutoMappedComponent();
        verify(businessNameComponent, never()).setComponentValue(anyString());
        verify(nonAutomappedComponent, never()).setComponentValue(anyString());
    }

    //BUSINESS TESTS//

    @Test
    public void testMapFields_businessName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.BUSINESS_NAME);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(BUSINESS_NAME_STRING);

        verify(business, times(1)).getName();
        verifyNoMoreInteractions(vendor);
        verifyNoMoreInteractions(customer);
    }

    @Test
    public void testMapFields_businessAddress_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.BUSINESS_ADDRESS);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(BUSINESS_ADDRESS_STRING);

        verify(business, times(1)).getAddress();
        verifyNoMoreInteractions(vendor);
        verifyNoMoreInteractions(customer);
    }

    @Test
    public void testMapFields_businessContactName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.BUSINESS_CONTACT_NAME);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(BUSINESS_CONTACT_NAME_STRING);

        verify(business, times(1)).getContactName();
        verifyNoMoreInteractions(vendor);
        verifyNoMoreInteractions(customer);
    }

    @Test
    public void testMapFields_businessPhone_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.BUSINESS_PHONE);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(BUSINESS_PHONE_STRING);

        verify(business, times(1)).getPhone();
        verifyNoMoreInteractions(vendor);
        verifyNoMoreInteractions(customer);
    }

    @Test
    public void testMapFields_businessCountry_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.BUSINESS_COUNTRY);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(BUSINESS_COUNTRY_STRING);
        verify(business, times(1)).getCountry();
        verifyNoMoreInteractions(vendor);
        verifyNoMoreInteractions(customer);
    }


    //VENDOR TESTS//

    @Test
    public void testMapFields_vendorCountry_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_COUNTRY);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(VENDOR_COUNTRY_STRING);
        verify(vendor, times(1)).getCountry();
        verifyNoMoreInteractions(business);
        verifyNoMoreInteractions(customer);
    }

    @Test
    public void testMapFields_vendorName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_NAME);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(VENDOR_NAME_STRING);
        verify(vendor, times(1)).getName();
        verifyNoMoreInteractions(business);
        verifyNoMoreInteractions(customer);
    }

    @Test
    public void testMapFields_vendorAddress_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_ADDRESS);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(VENDOR_ADDRESS_STRING);
        verify(vendor, times(1)).getAddress();
        verifyNoMoreInteractions(business);
        verifyNoMoreInteractions(customer);
    }

    @Test
    public void testMapFields_vendorContactName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_CONTACT_NAME);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(VENDOR_CONTACT_NAME_STRING);
        verify(vendor, times(1)).getContactName();
        verifyNoMoreInteractions(business);
        verifyNoMoreInteractions(customer);
    }

    @Test
    public void testMapFields_vendorPhone_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_PHONE);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(VENDOR_PHONE_STRING);
        verify(vendor, times(1)).getPhone();
        verifyNoMoreInteractions(business);
        verifyNoMoreInteractions(customer);
    }

    @Test
    public void testMapFields_vendorEmail_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_EMAIL);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(VENDOR_EMAIL_STRING);
        verify(vendor, times(1)).getEmail();
        verifyNoMoreInteractions(business);
        verifyNoMoreInteractions(customer);
    }

    @Test
    public void testMapFields_vendorTaxId_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_TAX_ID);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(VENDOR_TAXID_STRING);
        verify(vendor, times(1)).getTaxId();
        verifyNoMoreInteractions(business);
        verifyNoMoreInteractions(customer);
    }

    //CUSTOMER TESTS//

    @Test
    public void testMapFields_customerName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.CUSTOMER_NAME);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(CUSTOMER_NAME_STRING);

        verify(customer, times(1)).getName();
        verifyNoMoreInteractions(vendor);
        verifyNoMoreInteractions(business);
    }

    @Test
    public void testMapFields_customerAddress_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.CUSTOMER_ADDRESS);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(CUSTOMER_ADDRESS_STRING);

        verify(customer, times(1)).getAddress();
        verifyNoMoreInteractions(vendor);
        verifyNoMoreInteractions(business);
    }

    @Test
    public void testMapFields_customerContactName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.CUSTOMER_CONTACT_NAME);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(CUSTOMER_CONTACT_NAME_STRING);

        verify(customer, times(1)).getContactName();
        verifyNoMoreInteractions(vendor);
        verifyNoMoreInteractions(business);
    }

    @Test
    public void testMapFields_customerCountry_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.CUSTOMER_COUNTRY);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(CUSTOMER_COUNTRY_STRING);

        verify(customer, times(1)).getCountry();
        verifyNoMoreInteractions(vendor);
        verifyNoMoreInteractions(business);
    }

    @Test
    public void testMapFields_customerPhone_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.CUSTOMER_PHONE);

        verify(component, atLeastOnce()).getAutoMappedComponent();
        verify(component, times(1)).setComponentValue(CUSTOMER_PHONE_STRING);

        verify(customer, times(1)).getPhone();
        verifyNoMoreInteractions(vendor);
        verifyNoMoreInteractions(business);
    }

    //////////////////////////////////

    private DocComponentView makeComponentsAndMap(AutoMappedComponent field) {

        DocComponentView component = mockDocComponent();

        when(component.getAutoMappedComponent()).thenReturn(true);
        when(component.getAutoMappedField()).thenReturn(field);

        List<DocComponentView> components = Arrays.asList(component);

        mapper.mapFields(components,
                         customer,
                         vendor,
                         business);

        return component;
    }

    private DocComponentView mockDocComponent() {
        return mock(DocComponentView.class);
    }

}
