<%@ taglib prefix="sf" uri="http://www.springframework.org/tags/form" %>
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

<c:set var="fieldSeperator" value="~" scope="application"/>

<s:url var="customer_url" value="/customerSelect/"/>
<a href="${customer_url}">Customer</a>
&nbsp;->&nbsp;
<s:url var="products_url" value="/productSelect/"/>
<a href="${products_url}">Select products</a>
&nbsp;->&nbsp;
Enter data

<br/>
<br/>

<s:url var="toggleAutomapped" value="/dataEntry/toggleAutomapped"/>

<a href="${toggleAutomapped}">
    show/hide automapped
</a>

<form method="post"
      name="form"
      action="/docgen/dataEntry/setFields">

    <table>
        <c:forEach var="field"
                   items="${prepareFieldsController.docComponentViews}"
                   varStatus="fieldIndex">
            <tr>
                <td>
                    <label for="${field.name}">
                        <c:out value="${field.name}"/>
                    </label>
                </td>
                <td>
                    <c:if test="${field.text }">
                        <input name="${field.name}"
                               value="${field.stringValue}"
                               id="${field.name}"/>
                    </c:if>
                    <c:if test="${field.textArea}">
                        <textarea cols="30"
                                  rows="8"
                                  name="${field.name}"
                                  id="${field.name}"><c:out value="${field.stringValue}"/></textarea>
                    </c:if>
                    <c:if test="${field.table}">
                        <c:set var="table" value="${field}" scope="request"/>
                        <jsp:include page="table.jsp"/>
                    </c:if>
                </td>
            </tr>
        </c:forEach>
    </table>
    <input name="full" type="submit" value="Generate PDFs" onmousedown="setBlankTarget()"/>
    <%--<input name="partial" type="submit" value="partial" onmousedown="setSelfTarget()"/>--%>
</form>
