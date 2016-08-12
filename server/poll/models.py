from django.db import models


class Question(models.Model):
    created = models.DateTimeField(auto_now_add=True)
    updated = models.DateTimeField(auto_now=True)
    title = models.CharField(max_length=30)
    text = models.TextField()
    code = models.TextField()

    def __str__(self):
        return self.title

    class Meta:
        ordering = ('created', )


class Answer(models.Model):
    created = models.DateTimeField(auto_now_add=True)
    updated = models.DateTimeField(auto_now=True)
    question = models.ForeignKey(Question)
    text = models.CharField(max_length=30)
    order = models.PositiveIntegerField()

    def __str__(self):
        return self.text

    class Meta:
        ordering = ('order', )
