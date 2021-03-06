package org.lawrencebower.docgen.web.controller.data_entry;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.generator.utils.ChecksumUtils;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web.test_examples.factory.DocumentFactoryTestImpl;
import org.lawrencebower.docgen.web_logic.business.controler_business.data_entry.DataEntryCB;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document.component.TextComponentView;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-application-test-context.xml")
public class PrepareFieldsControllerTest {

    @Autowired
    DataEntryCB business;
    @Autowired
    ViewFactory viewFactory;
    @Autowired
    SessionSetupUtils sessionSetupUtils;
    @Autowired
    SessionData sessionData;
    PrepareFieldsController controller;

    @Autowired
    ChecksumUtils checksumUtils;

    @Before
    public void setUp() throws Exception {

        setupSessionData();

        controller = new PrepareFieldsController();
        controller.setSessionData(sessionData);
        controller.setBusiness(business);
    }

    private void setupSessionData() {
        sessionSetupUtils.setupSessionData(sessionData);
    }

    @Test
    public void testPrepareFields_validFields_correctDocumentNumberSet() throws Exception {
        controller.prepareFields();
        List<DocumentView> documentsAsList = sessionData.getDocumentsAsList();
        assertEquals(2, documentsAsList.size());
    }

    @Test
    public void testPrepareFields_validFields_correctDocumentsSet() throws Exception {
        controller.prepareFields();
        List<DocumentView> documents = sessionData.getDocumentsAsList();
        assertEquals(DocumentFactoryTestImpl.DOC_1_NAME, documents.get(0).getName());
        assertEquals(DocumentFactoryTestImpl.DOC_2_NAME, documents.get(1).getName());
    }

    @Test
    public void testPrepareFields_validFields_autoMappedFieldsMapped() throws Exception {
        controller.prepareFields();

        List<DocumentView> documents = sessionData.getDocumentsAsList();
        DocumentView doc1 = documents.get(0);
        assertEquals(DocumentFactoryTestImpl.DOC_1_NAME, doc1.getName());

        List<DocComponentView> components = doc1.getComponentViewsWithName(DocumentFactoryTestImpl.AUTO_MAPPED_EXAMPLE_FIELD);
        assertEquals("auto mapped field not found",
                     1,
                     components.size());

        ContactView vendor = viewFactory.getVendor();

        TextComponentView docComponentView = (TextComponentView) components.get(0);
        assertEquals("auto mapped field value not correctly mapped",
                     vendor.getAddress(),
                     docComponentView.getStringValue());
    }

    @Test
    public void testGetDocComponentViews_showAutoMappedOn_allComponentsReturned() throws Exception {
        controller.prepareFields();
        sessionData.setShowAutoMappedFields(true);
        List<DocComponentView> docComponentViews = controller.getDocComponentViews();
        assertEquals(7, docComponentViews.size());//all the components
    }

    @Test
    public void testGetDocComponentViews_showAutoMappedOff_nonMappedComponentsReturned() throws Exception {
        controller.prepareFields();
        sessionData.setShowAutoMappedFields(false);
        List<DocComponentView> docComponentViews = controller.getDocComponentViews();
        assertEquals(2, docComponentViews.size());//all the components
    }
}
