# Resubmission

Review round 1

Comment/Suggestion 1

Is there some reference about the method you can add in the Description
field in the form Authors (year) <doi:10.....>?

> Answer:
For now, there is no specific reference except for some standard statistical reference. 
The techniques used are standard statistical techniques. This library collates standard statistical
techniques (e.g., bootstrapping) suitable for analyzing A/B testing experiments. However,
I am preparing a short write up about the techniques mentioned here since these techniques
have been routinely used in several companies I have worked with. Once it is published I will
add it in the succeeding version.

Comment/Suggestion 2

Please omit the License file and specify

License: Apache-2.0

> Answer:
License file omitted and added Apache-2.0 in the description file. However, when I run `check()` it gives me a warning that it is a non-standard specification. So
I decided to retain what was provided by `use_apache_license()`.
