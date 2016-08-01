from django.contrib import admin
from .models import Question, Answer


class AnswerAdminInline(admin.TabularInline):
    model = Answer


class QuestionAdmin(admin.ModelAdmin):
    inlines = [AnswerAdminInline]


admin.site.register(Answer)
admin.site.register(Question, QuestionAdmin)
