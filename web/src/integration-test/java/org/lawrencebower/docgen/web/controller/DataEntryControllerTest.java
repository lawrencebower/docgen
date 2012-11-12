package org.lawrencebower.docgen.web.controller;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.data_entry.DataEntryCB;
import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactory;
import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactoryCodeImpl;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.handler.DispatcherServletWebRequest;

import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;
import static junit.framework.TestCase.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-application-test-context.xml")
public class DataEntryControllerTest {

    @Autowired
    DataEntryCB business;
    @Autowired
    ModelFactory modelFactory;
    SessionData sessionData;
    DataEntryController controller;

    @Before
    public void setUp() throws Exception {

        sessionData = new SessionData();
        setupSessionData();

        controller = new DataEntryController();
        controller.setSessionData(sessionData);
        controller.setBusiness(business);
    }

    private void setupSessionData() {
        setBusinessOnSession();
        setCustomerOnSession();
        setProductsOnSession();
    }

    private void setProductsOnSession() {
        ProductView product1 = modelFactory.getProduct(ModelFactoryCodeImpl.PRODUCT_ID_1);
        ProductView product2 = modelFactory.getProduct(ModelFactoryCodeImpl.PRODUCT_ID_2);
        sessionData.addSelectedProduct(product1);
        sessionData.addSelectedProduct(product2);
    }

    private void setCustomerOnSession() {
        ContactView selectedCustomer = modelFactory.getCustomer(ModelFactoryCodeImpl.CUSTOMER_ID_1);
        sessionData.setSelectedCustomer(selectedCustomer);
    }

    private void setBusinessOnSession() {
        ContactView selectedBusiness = modelFactory.getBusinessByCustomerName(ModelFactoryCodeImpl.CUSTOMER_ID_1);
        sessionData.setSelectedBusiness(selectedBusiness);
    }

    @Test
    public void testPrepareFields_validFields_correctDocumentNumberSet() throws Exception {
        controller.prepareFields();
        assertEquals(2, sessionData.getDocuments().size());
    }

    @Test
    public void testPrepareFields_validFields_correctDocumentsSet() throws Exception {
        controller.prepareFields();
        List<DocumentInfoView> documents = sessionData.getDocuments();
        assertEquals(ModelFactoryCodeImpl.DOC_1_NAME, documents.get(0).getName());
        assertEquals(ModelFactoryCodeImpl.DOC_2_NAME, documents.get(1).getName());
    }

    @Test
    public void testPrepareFields_validFields_autoMappedFieldsMapped() throws Exception {
        controller.prepareFields();

        List<DocumentInfoView> documents = sessionData.getDocuments();
        DocumentInfoView doc1 = documents.get(0);
        assertEquals(ModelFactoryCodeImpl.DOC_1_NAME, doc1.getName());

        List<DocComponentView> components = doc1.getComponentsWithName(ModelFactoryCodeImpl.AUTO_MAPPED_EXAMPLE_FIELD);
        assertEquals("auto mapped field not found",
                     1,
                     components.size());

        ContactView selectedBusiness = modelFactory.getBusinessByCustomerName(ModelFactoryCodeImpl.CUSTOMER_ID_1);
        assertEquals("auto mapped field value not correctly mapped",
                     selectedBusiness.getName(),
                     components.get(0).getComponentValue());
    }

    @Test
    public void testToggleShowAutomappedFields_showAutoMappedTrue_valueToggled() throws Exception {
        sessionData.setShowAutoMappedFields(true);
        controller.toggleShowAutomappedFields();
        assertFalse(sessionData.isShowAutoMappedFields());
    }

    @Test
    public void testToggleShowAutomappedFields_showAutoMappedFalse_valueToggled() throws Exception {
        sessionData.setShowAutoMappedFields(false);
        controller.toggleShowAutomappedFields();
        assertTrue(sessionData.isShowAutoMappedFields());
    }

    @Test
    public void testSubmitFields() throws Exception {

        WebRequest mockRequest = makeMockRequestWithParamMap();

        ByteArrayOutputStream outStream = new ByteArrayOutputStream();

        controller.prepareFields();
        controller.submitFields(mockRequest, outStream);

    }

    private WebRequest makeMockRequestWithParamMap() {
        WebRequest mockRequest = mock(WebRequest.class);
        Map<String, String[]> paramMap = new HashMap<>();
        paramMap.put(ModelFactoryCodeImpl.EXAMPLE_FIELD, new String[]{"hello"});
        when(mockRequest.getParameterMap()).thenReturn(paramMap);
        return mockRequest;
    }

    @Test
    public void testGetDocComponentViews() throws Exception {

    }
}
