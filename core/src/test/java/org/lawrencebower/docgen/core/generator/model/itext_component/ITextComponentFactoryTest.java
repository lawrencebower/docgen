package org.lawrencebower.docgen.core.generator.model.itext_component;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableComponent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.junit.Assert.assertTrue;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/core-test-context.xml"})
public class ITextComponentFactoryTest {

    @Autowired
    ITextComponentFactory factory;

    @Test
    public void testCreateTextComponent_textComponent_returnsValidIText() throws Exception {
        DocComponent component = new TextComponent("");
        ITextComponent iTextComponent = factory.createComponent(component);
        assertTrue(iTextComponent instanceof ITextTextComponent);
    }

    @Test
    public void testCreateTextComponent_tableComponent_returnsValidIText() throws Exception {
        DocComponent component = new LayoutTableComponent("");
        ITextComponent iTextComponent = factory.createComponent(component);
        assertTrue(iTextComponent instanceof ITextTableComponent);
    }

    @Test
    public void testCreateTextComponent_tableTextComponent_returnsValidIText() throws Exception {
        DocComponent component = new TableTextComponent("");
        ITextComponent iTextComponent = factory.createComponent(component);
        assertTrue(iTextComponent instanceof ITextTableTextComponent);
    }

    @Test
    public void testCreateTextComponent_imageComponent_returnsValidIText() throws Exception {
        DocComponent component = new ImageComponent("");
        ITextComponent iTextComponent = factory.createComponent(component);
        assertTrue(iTextComponent instanceof ITextImageComponent);
    }

    @Test
    public void lineComponent_returnsValidIText() throws Exception {
        DocComponent component = new LineComponent(100);
        ITextComponent iTextComponent = factory.createComponent(component);
        assertTrue(iTextComponent instanceof ITextLineComponent);
    }

    @Test
    public void newLineComponent_returnsValidIText() throws Exception {
        DocComponent component = new NewLineComponent();
        ITextComponent iTextComponent = factory.createComponent(component);
        assertTrue(iTextComponent instanceof ITextNewLineComponent);
    }

    @Test
    public void checkBoxComponent_returnsValidIText() throws Exception {
        DocComponent component = new CheckBoxComponent(true);
        ITextComponent iTextComponent = factory.createComponent(component);
        assertTrue(iTextComponent instanceof ITextCheckBoxComponent);
    }

}
