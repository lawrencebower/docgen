package org.lawrencebower.docgen.web_logic.business.mapping;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.mockito.Mockito.*;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class AutoMappedComponentTest {

    @Mock
    AutoMappedComponentInfo mappingInfo;

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

        mappingInfo = mock(AutoMappedComponentInfo.class);

        when(mappingInfo.getCustomerName()).thenReturn(CUSTOMER_NAME_STRING);
        when(mappingInfo.getCustomerContactName()).thenReturn(CUSTOMER_CONTACT_NAME_STRING);
        when(mappingInfo.getCustomerPhone()).thenReturn(CUSTOMER_PHONE_STRING);
        when(mappingInfo.getCustomerCountry()).thenReturn(CUSTOMER_COUNTRY_STRING);
        when(mappingInfo.getCustomerAddress()).thenReturn(CUSTOMER_ADDRESS_STRING);

        when(mappingInfo.getBusinessName()).thenReturn(BUSINESS_NAME_STRING);
        when(mappingInfo.getBusinessContactName()).thenReturn(BUSINESS_CONTACT_NAME_STRING);
        when(mappingInfo.getBusinessPhone()).thenReturn(BUSINESS_PHONE_STRING);
        when(mappingInfo.getBusinessCountry()).thenReturn(BUSINESS_COUNTRY_STRING);
        when(mappingInfo.getBusinessAddress()).thenReturn(BUSINESS_ADDRESS_STRING);

        when(mappingInfo.getVendorName()).thenReturn(VENDOR_NAME_STRING);
        when(mappingInfo.getVendorContactName()).thenReturn(VENDOR_CONTACT_NAME_STRING);
        when(mappingInfo.getVendorPhone()).thenReturn(VENDOR_PHONE_STRING);
        when(mappingInfo.getVendorCountry()).thenReturn(VENDOR_COUNTRY_STRING);
        when(mappingInfo.getVendorAddress()).thenReturn(VENDOR_ADDRESS_STRING);
        when(mappingInfo.getVendorEmail()).thenReturn(VENDOR_EMAIL_STRING);
        when(mappingInfo.getVendorTaxId()).thenReturn(VENDOR_TAXID_STRING);

    }

    @Test
    public void testMapComponent_businessName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.BUSINESS_NAME);

        verify(component, times(1)).setComponentValue(BUSINESS_NAME_STRING);

        verify(mappingInfo, times(1)).getBusinessName();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_businessAddress_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.BUSINESS_ADDRESS);

        verify(component, times(1)).setComponentValue(BUSINESS_ADDRESS_STRING);

        verify(mappingInfo, times(1)).getBusinessAddress();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_businessContactName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.BUSINESS_CONTACT_NAME);

        verify(component, times(1)).setComponentValue(BUSINESS_CONTACT_NAME_STRING);

        verify(mappingInfo, times(1)).getBusinessContactName();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_businessPhone_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.BUSINESS_PHONE);

        verify(component, times(1)).setComponentValue(BUSINESS_PHONE_STRING);

        verify(mappingInfo, times(1)).getBusinessPhone();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_businessCountry_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.BUSINESS_COUNTRY);

        verify(component, times(1)).setComponentValue(BUSINESS_COUNTRY_STRING);
        verify(mappingInfo, times(1)).getBusinessCountry();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }


    //VENDOR TESTS//

    @Test
    public void testMapComponent_vendorCountry_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_COUNTRY);

        verify(component, times(1)).setComponentValue(VENDOR_COUNTRY_STRING);
        verify(mappingInfo, times(1)).getVendorCountry();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_vendorName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_NAME);

        verify(component, times(1)).setComponentValue(VENDOR_NAME_STRING);
        verify(mappingInfo, times(1)).getVendorName();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_vendorAddress_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_ADDRESS);

        verify(component, times(1)).setComponentValue(VENDOR_ADDRESS_STRING);
        verify(mappingInfo, times(1)).getVendorAddress();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_vendorContactName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_CONTACT_NAME);

        verify(component, times(1)).setComponentValue(VENDOR_CONTACT_NAME_STRING);
        verify(mappingInfo, times(1)).getVendorContactName();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_vendorPhone_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_PHONE);

        verify(component, times(1)).setComponentValue(VENDOR_PHONE_STRING);
        verify(mappingInfo, times(1)).getVendorPhone();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_vendorEmail_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_EMAIL);

        verify(component, times(1)).setComponentValue(VENDOR_EMAIL_STRING);
        verify(mappingInfo, times(1)).getVendorEmail();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_vendorTaxId_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.VENDOR_TAX_ID);

        verify(component, times(1)).setComponentValue(VENDOR_TAXID_STRING);
        verify(mappingInfo, times(1)).getVendorTaxId();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    //CUSTOMER TESTS//

    @Test
    public void testMapComponent_customerName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.CUSTOMER_NAME);

        verify(component, times(1)).setComponentValue(CUSTOMER_NAME_STRING);

        verify(mappingInfo, times(1)).getCustomerName();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_customerAddress_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.CUSTOMER_ADDRESS);

        verify(component, times(1)).setComponentValue(CUSTOMER_ADDRESS_STRING);

        verify(mappingInfo, times(1)).getCustomerAddress();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_customerContactName_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.CUSTOMER_CONTACT_NAME);

        verify(component, times(1)).setComponentValue(CUSTOMER_CONTACT_NAME_STRING);

        verify(mappingInfo, times(1)).getCustomerContactName();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_customerCountry_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.CUSTOMER_COUNTRY);

        verify(component, times(1)).setComponentValue(CUSTOMER_COUNTRY_STRING);

        verify(mappingInfo, times(1)).getCustomerCountry();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    @Test
    public void testMapComponent_customerPhone_correctFieldMapped() {

        DocComponentView component = makeComponentsAndMap(AutoMappedComponent.CUSTOMER_PHONE);

        verify(component, times(1)).setComponentValue(CUSTOMER_PHONE_STRING);

        verify(mappingInfo, times(1)).getCustomerPhone();
        verifyNoMoreInteractions(mappingInfo);
        verifyNoMoreInteractions(mappingInfo);
    }

    //////////////////////////////////

    private DocComponentView makeComponentsAndMap(AutoMappedComponent field) {

        DocComponentView component = mockDocComponent();

        when(component.isAutoMappedComponent()).thenReturn(true);

        AutoMappedComponent.mapComponent(component,
                                         field,
                                         mappingInfo);

        return component;
    }

    private DocComponentView mockDocComponent() {
        return mock(DocComponentView.class);
    }

}
