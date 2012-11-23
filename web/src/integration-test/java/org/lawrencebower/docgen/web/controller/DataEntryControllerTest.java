package org.lawrencebower.docgen.web.controller;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.generator.utils.ChecksumUtils;
import org.lawrencebower.docgen.doc_examples.ModelFactoryCodeImpl;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.data_entry.DataEntryCB;
import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactory;
import org.lawrencebower.docgen.web_logic.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.TextComponentView;
import org.lawrencebower.docgen.web_logic.view.product.Product;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.context.request.WebRequest;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
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

    @Autowired
    @Qualifier("webTestInputRoot")
    String testInputRoot;

    @Autowired
    @Qualifier("pdfOutputRoot")
    String testOutputRoot;

    String testInputPath;

    @Autowired
    ChecksumUtils checksumUtils;

    @Before
    public void setUp() throws Exception {

        sessionData = new SessionData();
        setupSessionData();

        controller = new DataEntryController();
        controller.setSessionData(sessionData);
        controller.setBusiness(business);

        setupInputPath();
    }

    /**
     * sets the directory where the expected file templates are written
     */
    private void setupInputPath() {
        String packageName = getClass().getPackage().getName();
        packageName = packageName.replaceAll("\\.", "\\\\");

        testInputPath = testInputRoot + packageName + File.separator;
    }

    private void setupSessionData() {
        setBusinessOnSession();
        setCustomerOnSession();
        setProductsOnSession();
    }

    private void setProductsOnSession() {
        Product product1 = modelFactory.getProduct(ModelFactoryCodeImpl.PRODUCT_ID_1);
        Product product2 = modelFactory.getProduct(ModelFactoryCodeImpl.PRODUCT_ID_2);
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

        List<DocComponentView> components = doc1.getComponentViewsWithName(ModelFactoryCodeImpl.AUTO_MAPPED_EXAMPLE_FIELD);
        assertEquals("auto mapped field not found",
                     1,
                     components.size());

        ContactView selectedBusiness = modelFactory.getBusinessByCustomerName(ModelFactoryCodeImpl.CUSTOMER_ID_1);
        TextComponentView docComponentView = (TextComponentView) components.get(0);
        assertEquals("auto mapped field value not correctly mapped",
                     selectedBusiness.getName(),
                     docComponentView.getStringValue());
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

        verifyExpectedPDFFilesWritten();

        verifyBytesWrittenToStreamCorrect(outStream);
    }

    /**
     * Checks that the bytes written to the output stream are those of the concatenated file written in
     * the output directory
     */
    private void verifyBytesWrittenToStreamCorrect(ByteArrayOutputStream outStream) throws IOException {

        File expectedConcatenatedFile = new File(testOutputRoot + ViewConstants.CONCATENATED_FILE_NAME);
        byte[] expectedFileBytes = FileUtils.readFileToByteArray(expectedConcatenatedFile);
        String expectedChecksum = checksumUtils.getChecksumFromBytes(expectedFileBytes);

        String httpResponseChecksum = checksumUtils.getChecksumFromBytes(outStream.toByteArray());

        assertEquals(expectedChecksum, httpResponseChecksum);
    }

    private void verifyExpectedPDFFilesWritten() {
        checkConcatenatedFiles();
        checkDoc1();
        checkDoc2();
    }

    private void checkConcatenatedFiles() {
        File expectedConcatenatedFile = new File(testInputPath + "EXPECTED_CONCATENATED_FILE.pdf");
        File createdConcatenatedFile = new File(testOutputRoot + ViewConstants.CONCATENATED_FILE_NAME);
        checkFilesAreSame(expectedConcatenatedFile, createdConcatenatedFile);
    }

    private void checkDoc1() {
        File expectedConcatenatedFile = new File(testInputPath + "EXPECTED_DOC1.pdf");
        File createdConcatenatedFile = new File(testOutputRoot + ModelFactoryCodeImpl.DOC_1_NAME + ".pdf");
        checkFilesAreSame(expectedConcatenatedFile, createdConcatenatedFile);
    }

    private void checkDoc2() {
        File expectedConcatenatedFile = new File(testInputPath + "EXPECTED_DOC2.pdf");
        File createdConcatenatedFile = new File(testOutputRoot + ModelFactoryCodeImpl.DOC_2_NAME + ".pdf");
        checkFilesAreSame(expectedConcatenatedFile, createdConcatenatedFile);
    }

    private void checkFilesAreSame(File expectedConcatenatedFile, File createdConcatenatedFile) {
        boolean filesAreSame =
                checksumUtils.filteredFileChecksumsAreSame(expectedConcatenatedFile, createdConcatenatedFile);
        assertTrue(filesAreSame);
    }

    private WebRequest makeMockRequestWithParamMap() {
        WebRequest mockRequest = mock(WebRequest.class);
        Map<String, String[]> paramMap = new HashMap<>();
        paramMap.put(ModelFactoryCodeImpl.EXAMPLE_FIELD, new String[]{"hello"});
        when(mockRequest.getParameterMap()).thenReturn(paramMap);
        return mockRequest;
    }

    @Test
    public void testGetDocComponentViews_showAutoMappedOn_allComponentsReturned() throws Exception {
        controller.prepareFields();
        sessionData.setShowAutoMappedFields(true);
        List<DocComponentView> docComponentViews = controller.getDocComponentViews();
        assertEquals(23, docComponentViews.size());//all the components
    }

    @Test
    public void testGetDocComponentViews_showAutoMappedOff_nonMappedComponentsReturned() throws Exception {
        controller.prepareFields();
        sessionData.setShowAutoMappedFields(false);
        List<DocComponentView> docComponentViews = controller.getDocComponentViews();
        assertEquals(4, docComponentViews.size());//all the components
    }
}
