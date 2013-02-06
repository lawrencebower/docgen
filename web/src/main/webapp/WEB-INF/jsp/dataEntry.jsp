<%@ taglib prefix="sf" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>
<%@ include file="/WEB-INF/jsp/include.jsp" %>


<script type="text/javascript">
    function setBlankTarget() {
        document.form.target = "_blank";
    }
    function setSelfTarget() {
        document.form.target = "_self";
    }
</script>

<jsp:useBean id="sessionData"
             scope="session"
             type="org.lawrencebower.docgen.web.model.SessionData"/>

<jsp:useBean id="prepareFieldsController"
             scope="session"
             type="org.lawrencebower.docgen.web.controller.data_entry.PrepareFieldsController"/>

<jsp:useBean id="dataEntryBean"
             scope="request"
             type="org.lawrencebower.docgen.web_model.view.document.binding.DataEntryBindBean"/>

<c:set var="fieldSeperator" value="~" scope="application"/>

<s:url var="customer_url" value="/customerSelect"/>
<a href="${customer_url}">Customer</a>
&nbsp;->&nbsp;
<s:url var="products_url" value="/productSelect"/>
<a href="${products_url}">Select products</a>
&nbsp;->&nbsp;
Enter data

<br/>
<br/>

<s:url var="toggleAutomapped" value="/dataEntry/toggleAutomapped"/>

<a href="${toggleAutomapped}">
    show/hide automapped
</a>

<sf:form method="post"
         name="form"
         action="/docgen/dataEntry/setFields"
         modelAttribute="dataEntryBean">

    <table>
        <c:forEach var="field"
                   items="${prepareFieldsController.docComponentViews}"
                   varStatus="fieldIndex">

            <input id="${field.name}"
                   name="components[${fieldIndex.index}].name"
                   value="${field.name}"
                   type="hidden"/>

            <tr>
                <td>
                    <label for="${field.name}">
                        <c:out value="${field.name}"/>
                    </label>
                </td>
                <td>
                    <c:if test="${field.text}">
                        <input id="${field.name}"
                               name="components[${fieldIndex.index}].value"
                               value="${field.stringValue}"/>
                    </c:if>
                    <c:if test="${field.textArea}">
                        <textarea cols="30"
                                  rows="8"
                                  name="components[${fieldIndex.index}].value"
                                  id="${field.name}"><c:out value="${field.stringValue}"/></textarea>
                    </c:if>
                    <c:if test="${field.table}">
                        <c:set var="table" value="${field}" scope="request"/>
                        <c:set var="fieldName" value="${field.name}" scope="request"/>
                        <c:set var="fieldIndex" value="${fieldIndex.index}" scope="request"/>
                        <jsp:include page="table.jsp"/>
                    </c:if>
                </td>
            </tr>
        </c:forEach>
    </table>
    <input type="submit" value="Generate PDFs" onmousedown="setBlankTarget()"/>
    <%--<input name="partial" type="submit" value="partial" onmousedown="setSelfTarget()"/>--%>
</sf:form>
