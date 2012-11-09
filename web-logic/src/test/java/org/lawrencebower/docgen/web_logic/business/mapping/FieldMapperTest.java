package org.lawrencebower.docgen.web_logic.business.mapping;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponentFactory;
import org.lawrencebower.docgen.web_model.view.constants.AutoMappedField;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentViewFactory;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.*;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class FieldMapperTest {

    @Autowired
    FieldMapper fieldMapper;
    @Autowired
    CustomComponentFactory customComponentFactory;
    @Autowired
    DocComponentViewFactory componentViewFactory;

    private final String fieldName1 = "fieldName1";
    private final String fieldName2 = "fieldName2";
    private final String fieldName3 = "fieldName3";

    private final String value1 = "value1";
    private final String value2 = "value2";
    private final String value3 = "value3";

    TextComponent component1;
    TextComponent component2;
    TextComponent component3;
    TextComponent component4;

    @Before
    public void setup() {
        component1 = mock(TextComponent.class);
    }

    @Test
    public void testMapFieldValuesToComponents_validParams_correctComponentSet() {

        Map<String, String[]> paramMap = makeParameterMap();

        //define mock behaviour
        when(component1.getName()).thenReturn(fieldName1);
        component2 = mock(TextComponent.class);
        when(component2.getName()).thenReturn("some value not in the params");
        component3 = mock(TextComponent.class);
        when(component3.getName()).thenReturn("some value not in the params");
        component4 = mock(TextComponent.class);
        when(component4.getName()).thenReturn(fieldName1);

        //add mocks to Documents
        List<DocumentInfoView> documentInfoViews = addMocksToDocuments();

        //run mapping
        fieldMapper.mapFieldValuesToComponents(paramMap, documentInfoViews);

        //verify
        verify(component1, times(1)).setText(value1);

        verify(component2, atLeastOnce()).getName();
        verify(component2, never()).setText(anyString());

        verify(component3, atLeastOnce()).getName();
        verify(component3, never()).setText(anyString());

        verify(component4, times(1)).setText(value1);
    }

    @Test
    public void testMapFieldValuesToComponents_excludedField_excludedFieldSkipped() {

        String excludedTokenName = "full";
        Map<String, String[]> paramMap = makeParameterMap();
        paramMap.put(excludedTokenName,new String[]{"this value should not be set"});


        //define mock behaviour
        when(component1.getName()).thenReturn(fieldName1);
        component2 = mock(TextComponent.class);
        when(component2.getName()).thenReturn(excludedTokenName);
        component3 = mock(TextComponent.class);
        when(component3.getName()).thenReturn("some value not in the params");
        component4 = mock(TextComponent.class);
        when(component4.getName()).thenReturn("some value not in the params");

        //add mocks to Documents
        List<DocumentInfoView> documentInfoViews = addMocksToDocuments();

        //run mapping
        fieldMapper.mapFieldValuesToComponents(paramMap, documentInfoViews);

        //verify
        verify(component1, times(1)).setText(value1);
        verify(component2, atLeastOnce()).getName();
        verify(component2, never()).setText(anyString());
        verify(component3, atLeastOnce()).getName();
        verify(component3, never()).setText(anyString());
        verify(component4, atLeastOnce()).getName();
        verify(component4, never()).setText(anyString());
    }

    @Test
    public void testMapFieldValuesToComponents_missingValuesInParamMap_throwsError() {

        try {
            Map<String, String[]> paramMap = makeParameterMap();
            paramMap.put("token with no values", new String[]{});

            //define mock behaviour
            when(component1.getName()).thenReturn(fieldName1);
            component2 = mock(TextComponent.class);
            when(component2.getName()).thenReturn(fieldName2);
            component3 = mock(TextComponent.class);
            when(component3.getName()).thenReturn(fieldName3);
            component4 = mock(TextComponent.class);
            when(component4.getName()).thenReturn(fieldName1);

            //add mocks to Documents
            List<DocumentInfoView> documentInfoViews = addMocksToDocuments();

            //run mapping
            fieldMapper.mapFieldValuesToComponents(paramMap, documentInfoViews);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals("No values bound to field 'token with no values'", message);
        }
    }

    @Test
    public void testMapFieldValuesToComponents_moreThanOneBoundValueInParamMap_throwsError() {

        try {
            Map<String, String[]> paramMap = makeParameterMap();
            paramMap.put("token with no values", new String[]{"value1","value2"});

            //define mock behaviour
            when(component1.getName()).thenReturn(fieldName1);
            component2 = mock(TextComponent.class);
            when(component2.getName()).thenReturn(fieldName2);
            component3 = mock(TextComponent.class);
            when(component3.getName()).thenReturn(fieldName3);
            component4 = mock(TextComponent.class);
            when(component4.getName()).thenReturn(fieldName1);

            //add mocks to Documents
            List<DocumentInfoView> documentInfoViews = addMocksToDocuments();

            //run mapping
            fieldMapper.mapFieldValuesToComponents(paramMap, documentInfoViews);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals("more than 1 value bound to field 'token with no values'", message);
        }
    }

    //UTIL METHODS//

    private List<DocumentInfoView> addMocksToDocuments() {
        DocumentInfoView documentInfoView1 = makeDocInfoView();
        addComponentViewToDoc(documentInfoView1, component1);
        addComponentViewToDoc(documentInfoView1, component2);

        DocumentInfoView documentInfoView2 = makeDocInfoView();
        addComponentViewToDoc(documentInfoView2, component3);
        addComponentViewToDoc(documentInfoView2, component4);

        return Arrays.asList(documentInfoView1, documentInfoView2);
    }

    private void addComponentViewToDoc(DocumentInfoView documentInfoView, TextComponent component) {
        DocComponentView componentView = componentViewFactory.createTextComponentView(component);
        documentInfoView.addComponentView(componentView);
    }

    private DocumentInfoView makeDocInfoView() {
        return new DocumentInfoView(mock(DocumentInfo.class));
    }

    private Map<String, String[]> makeParameterMap() {
        Map<String, String[]> paramMap = new HashMap<>();
        paramMap.put(fieldName1, new String[]{value1});
        paramMap.put(fieldName2, new String[]{value2});
        paramMap.put(fieldName3, new String[]{value3});
        return paramMap;
    }

}
