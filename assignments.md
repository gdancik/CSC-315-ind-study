---
layout: page
title: Assignments 
permalink: /assignments/
order: 3
exclude_from_nav: false
---

<style>
.due {
    background-color: yellow
}
</style>

<p style = 'color:red;font-size:104%'>Note: All assignments must be submitted through <a href = "https://easternct.blackboard.com/">Blackboard</a> unless stated otherwise. Assignments must be submitted in the correct format, which is a zipped version of an HTML R Notebook with none of the question numbers modified. Unless stated otherwise, you are also not allowed to use any R functions or concepts not discussed in class. 

</p>
{% comment %}
{% endcomment %}

* Install <i>R/RStudio</i> and the required packages by following the instructions on the [Course Info]({{ site.baseurl }}/info/) page 
* [Lab #1]({{ site.baseurl }}/data/hw/Lab1.R) 
* [Lab #2]({{ site.baseurl }}/data/hw/Lab2.R) 
* [Lab #3]({{ site.baseurl }}/data/hw/Lab3.R) 
<hr style = "margin-top:-10px">
* [Lab #4]({{ site.baseurl }}/data/hw/Lab4.R) 
* [Lab #5]({{ site.baseurl }}/data/hw/Lab5.R) 
* [Lab #6]({{ site.baseurl }}/data/hw/Lab6.R)
* [Lab #7]({{ site.baseurl }}/data/hw/Lab7.R) 
<hr style = "margin-top:-10px">
* [Lab #8]({{ site.baseurl }}/data/hw/Lab8.R) 
* [Lab #9]({{ site.baseurl }}/data/hw/Lab9_contrasts.R)
* [Lab #10]({{ site.baseurl }}/data/hw/Lab10.R)
<hr style = "margin-top:-10px">
* [Final Project]({{ site.baseurl }}/data/hw/Project.pdf) 
    * [Limma with paired samples]({{ site.baseurl }}/data/hw/Limma_with_paired_samples.R)
    * [Example of DE genes between males/females]({{ site.baseurl }}/data/hw/SexGenes.xlsx)
    * [Real world example](https://pubmed.ncbi.nlm.nih.gov/30573692/)
{% comment %}
{% endcomment %}

<script>
const pattern = RegExp('Due:.*([0-9]{2}/[0-9]+/[0-9]{4})');
elements = document.getElementsByTagName('li');

for (el of elements) {
        var res = pattern.exec(el.innerText);
        if (res != null && res.length >= 2) {
                if (new Date(res[1]) >= new Date()) {
                        el.className = 'due';
                }
        }
}
</script>
