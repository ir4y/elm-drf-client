from rest_framework import routers
from .viewsets import AnswerViewSet, QuestionViewSet


router = routers.DefaultRouter()
router.register(r'answer', AnswerViewSet)
router.register(r'question', QuestionViewSet)
